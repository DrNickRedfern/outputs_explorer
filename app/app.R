library(shiny)
library(bslib)
library(ggplot2)
library(dplyr)
library(readr)
library(here)
library(tidyr)
library(plotly)
library(janitor)
library(stringr)
library(DT)

min_year <- 2021
max_year <- 2025
years <- min_year:max_year

# df <- read_csv(here("app", "data", "stacked_data.csv"), show_col_types = FALSE) |>
df <- read_csv("https://raw.githubusercontent.com/DrNickRedfern/outputs_explorer/refs/heads/main/app/data/stacked_data.csv", show_col_types = FALSE)
df <- df |> 
  mutate(year = as.character(year)) |>
  arrange(publisher)
publishers <- unique(df$publisher)

ui <- page_sidebar(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  title = "Outputs Explorer",

  sidebar = sidebar(
    width = "20%",
    title = "Selection Controls",
    selectInput(
      "publisher",
      "Select publisher:",
      choices = c("All", publishers)
    ),
    selectInput(
      "journal",
      "Select journal:",
      choices = c("All")
    ),
    hr(),
    conditionalPanel(
      condition = "output.has_true_values",
      radioButtons(
        "corresponding_filter",
        "Choose which outputs to show:",
        choices = c("All" = FALSE, "Corresponding authors only" = TRUE)
      )
    )
  ),

  navset_card_underline(
    nav_panel(
      "Summary",
      layout_columns(
        col_widths = c(7, 5),
        card(
          card_header("Open Access"),
          card_body(
            plotlyOutput("openaccess_barChart")
          )
        ),
        card(
          card_header("Summary"),
          card_body(
            DTOutput("tbl_summary")
          )
        )
      )
    ),
    nav_panel("Data table", DTOutput("data_tbl"))
  )
)

server <- function(input, output, session) {
  # Update subcategory choices based on selected category
  observe({
    if (input$publisher == "All") {
      updateSelectInput(
        session,
        "journal",
        choices = c("All"),
        selected = "All"
      )
    } else {
      updateSelectInput(
        session,
        "journal",
        choices = c(
          "All",
          unique(df$journal[which(df$publisher == input$publisher)])
        ),
        selected = "All"
      )
    }
  })

  # Filter data based on selections
  pj_filtered_data <- reactive({
    if (input$publisher == "All") {
      return(df)
    } else if (input$journal == "All") {
      return(df |> filter(publisher == input$publisher))
    } else {
      return(
        df |> filter(publisher == input$publisher, journal == input$journal)
      )
    }
  })

  has_true_values <- reactive({
    any(pj_filtered_data()$uob_is_corresponding == TRUE)
  })

  output$has_true_values <- reactive({
    has_true_values()
  })
  outputOptions(output, "has_true_values", suspendWhenHidden = FALSE)

  output$test_input <- reactive({
    input$corresponding_filter
  })

  filtered_data <- reactive({
    dat <- pj_filtered_data()

    # Only apply the true/false filter if TRUE values exist and corresponding is TRUE
    if (input$corresponding_filter) {
      dat <- dat |> filter(uob_is_corresponding)
    }
    dat
  })

  # Data for infoboxes
  output$year_range <- renderText({
    year_range_min <- min(filtered_data()$year)
    year_range_max <- max(filtered_data()$year)
    if (year_range_min == year_range_max) {
      return(year_range_min)
    } else {
      return(paste0(year_range_min, " - ", year_range_max))
    }
  })

  output$journals <- renderText({
    format(n_distinct(filtered_data()$journal), big.mark = ",")
  })

  output$outputs <- renderText({
    format(n_distinct(filtered_data()$doi), big.mark = ",")
  })

  output$corresponding <- renderText({
    paste0(
      100 *
        round(
          sum(filtered_data()$uob_is_corresponding) /
            n_distinct(filtered_data()$doi),
          3
        ),
      "%"
    )
  })

  output$pct_oa <- renderText({
    paste0(
      100 *
        round(
          sum(filtered_data()$open_access != "closed") /
            n_distinct(filtered_data()$doi),
          3
        ),
      "%"
    )
  })

  # Summary table instead of info boxes
  summary_tbl <- reactive({
    year_range_min <- min(filtered_data()$year)
    year_range_max <- max(filtered_data()$year)
    if (year_range_min == year_range_max) {
      year_range <- year_range_min
    } else {
      year_range <- paste0(year_range_min, " - ", year_range_max)
    }

    if (input$publisher == "All") {
      publishers <- format(
        n_distinct(filtered_data()$publisher),
        big.mark = ","
      )
      publisher_name <- "Publishers"
    } else {
      publishers <- input$publisher
      publisher_name <- "Publisher"
    }

    if (input$journal == "All") {
      journals <- format(n_distinct(filtered_data()$journal), big.mark = ",")
      journal_name <- "Journals"
    } else {
      journals <- input$journal
      journal_name <- "Journal"
    }

    outputs <- format(n_distinct(filtered_data()$doi), big.mark = ",")

    pct_corresponding <- 100 *
      round(
        sum(filtered_data()$uob_is_corresponding) /
          n_distinct(filtered_data()$doi),
        3
      )

    pct_oa <- 100 *
      round(
        sum(filtered_data()$open_access != "closed") /
          n_distinct(filtered_data()$doi),
        3
      )

    data.frame(
      Variable = c(
        "Year range",
        publisher_name,
        journal_name,
        "Total outputs",
        "Open access (%)",
        "Corresponding authors (%)"
      ),
      Value = c(
        year_range,
        publishers,
        journals,
        as.character(outputs),
        as.character(pct_oa),
        as.character(pct_corresponding)
      )
    )
  })

  output$tbl_summary <- renderDT(
    datatable(
      summary_tbl(),
      rownames = FALSE,
      extensions = "Buttons",
      options = list(
        dom = "B",
        buttons = c("csv", "excel")
      )
    )
  )

  # Create bar chart
  output$openaccess_barChart <- renderPlotly({
    data <- filtered_data()

    if (input$publisher == "All") {
      # When all publishers are selected, aggregate by year only
      # plot_data <- data |>
      #   group_by(year) |>
      #   tally(n = "Total")

      plot_data <- data |>
        group_by(open_access, year) |>
        tally(n = "outputs") |>
        ungroup() |>
        mutate(
          open_access = str_to_title(open_access),
          open_access = factor(
            open_access,
            levels = c("Bronze", "Gold", "Green", "Hybrid", "Diamond", "Closed")
          )
        ) |>
        clean_names(case = "sentence")

      year_totals <- plot_data |>
        group_by(Year) |>
        summarise(total = sum(Outputs)) |>
        ungroup()

      if (max(year_totals$total) < 20) {
        major_breaks <- seq(0, 20, 4)
        spacer <- 4
      } else if (max(year_totals$total) < 100) {
        major_breaks <- seq(0, 100, 10)
        spacer <- 10
      } else if (max(year_totals$total) < 1000) {
        major_breaks <- seq(0, 1000, 100)
        spacer <- 25
      } else {
        major_breaks <- seq(0, 10000, 1000)
        spacer <- 50
      }

      p <- ggplot() +
        geom_text(
          data = year_totals,
          aes(x = Year, y = max(total) + spacer, label = total),
          size = 3.5
        ) +
        geom_bar(
          data = plot_data,
          aes(x = Year, y = Outputs, fill = `Open access`),
          stat = "identity"
        ) +
        scale_y_continuous(breaks = major_breaks) +
        scale_fill_manual(
          breaks = c("Bronze", "Gold", "Green", "Hybrid", "Diamond", "Closed"),
          values = viridis::viridis(6)
        ) +
        labs(title = "All publishers", x = "Year", y = "Outputs") +
        theme_minimal() +
        theme(legend.position = "bottom")
      ggplotly(p)
    } else if (input$journal == "All") {
      plot_data <- data |>
        group_by(publisher, open_access, year) |>
        tally(n = "outputs") |>
        ungroup() |>
        complete(
          publisher,
          open_access,
          year,
          fill = list(outputs = 0)
        ) |>
        mutate(
          open_access = str_to_title(open_access),
          open_access = factor(
            open_access,
            levels = c("Bronze", "Gold", "Green", "Hybrid", "Diamond", "Closed")
          )
        ) |>
        clean_names(case = "sentence")

      # This will only add zeroes for years in which there are no publications whatsoever,
      # everything else is the responsibility of the above complete command
      for (i in seq_along(years)) {
        current_year <- years[i]
        if (!current_year %in% plot_data$Year) {
          df_temp <- data.frame(
            Publisher = plot_data$Publisher,
            Year = as.character(current_year),
            Outputs = 0
          ) |>
            distinct()
          plot_data <- bind_rows(plot_data, df_temp)
        }
      }

      year_totals <- plot_data |>
        group_by(Year) |>
        summarise(total = sum(Outputs)) |>
        ungroup()

      if (max(year_totals$total) < 20) {
        major_breaks <- seq(0, 20, 4)
        spacer <- 2
      } else if (max(year_totals$total) < 100) {
        major_breaks <- seq(0, 100, 10)
        spacer <- 5
      } else if (max(year_totals$total) < 1000) {
        major_breaks <- seq(0, 1000, 100)
        spacer <- 10
      } else {
        major_breaks <- seq(0, 10000, 1000)
        spacer <- 50
      }

      # Dummy data to stop NA appearing in ggplotly legend
      plot_data <- plot_data |>
        replace_na(list(`Open access` = plot_data$`Open access`[1]))

      p <- ggplot() +
        geom_text(
          data = year_totals,
          aes(x = Year, y = max(total) + spacer, label = total),
          size = 3.5
        ) +
        geom_bar(
          data = plot_data,
          aes(x = Year, y = Outputs, fill = `Open access`),
          stat = "identity"
        ) +
        scale_y_continuous(breaks = major_breaks) +
        scale_fill_manual(
          breaks = c("Bronze", "Gold", "Green", "Hybrid", "Diamond", "Closed"),
          values = viridis::viridis(6)
        ) +
        labs(
          title = input$publisher,
          x = "Year",
          y = "Outputs"
        ) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          panel.grid.minor.y = element_blank()
        )
      ggplotly(p)
    } else {
      plot_data <- data |>
        group_by(publisher, journal, open_access, year) |>
        tally(n = "outputs") |>
        ungroup() |>
        complete(
          publisher,
          journal,
          open_access,
          year,
          fill = list(outputs = 0)
        ) |>
        mutate(
          open_access = str_to_title(open_access),
          open_access = factor(
            open_access,
            levels = c("Bronze", "Gold", "Green", "Hybrid", "Diamond", "Closed")
          )
        ) |>
        clean_names(case = "sentence")

      # This will only add zeroes for years in which there are no publications whatsoever,
      # everything else is the responsibility of the above complete command
      for (i in seq_along(years)) {
        current_year <- years[i]
        if (!current_year %in% plot_data$Year) {
          df_temp <- data.frame(
            Publisher = plot_data$Publisher,
            Journal = plot_data$Journal,
            Year = as.character(current_year),
            Outputs = 0
          ) |>
            distinct()
          plot_data <- bind_rows(plot_data, df_temp)
        }
      }

      year_totals <- plot_data |>
        group_by(Year) |>
        summarise(total = sum(Outputs)) |>
        ungroup()

      if (max(year_totals$total) < 20) {
        major_breaks <- seq(0, 20, 4)
        spacer <- 2
      } else if (max(year_totals$total) < 100) {
        major_breaks <- seq(0, 100, 10)
        spacer <- 5
      } else if (max(year_totals$total) < 1000) {
        major_breaks <- seq(0, 1000, 100)
        spacer <- 10
      } else {
        major_breaks <- seq(0, 10000, 1000)
        spacer <- 50
      }

      plot_data <- plot_data |>
        replace_na(list(`Open access` = plot_data$`Open access`[1]))

      p <- ggplot() +
        geom_text(
          data = year_totals,
          aes(x = Year, y = max(total) + spacer, label = total),
          size = 3.5
        ) +
        geom_bar(
          data = plot_data,
          aes(x = Year, y = Outputs, fill = `Open access`),
          stat = "identity"
        ) +
        scale_y_continuous(breaks = major_breaks) +
        scale_fill_manual(
          breaks = c("Bronze", "Gold", "Green", "Hybrid", "Diamond", "Closed"),
          values = viridis::viridis(6)
        ) +
        labs(
          title = input$journal,
          x = "Year",
          y = "Outputs"
        ) +
        theme_minimal() +
        theme(
          legend.position = "bottom",
          panel.grid.minor.y = element_blank()
        )

      ggplotly(p)
    }
  })

  tbl_data <- reactive({
    year_trend <- filtered_data() |>
      select(-doi) |>
      group_by(publisher, journal, year) |>
      tally(n = "outputs") |>
      ungroup() |>
      complete(nesting(publisher, journal), year, fill = list(outputs = 0))

    if (input$publisher != "All") {
      for (i in seq_along(years)) {
        current_year <- years[i]
        if (!current_year %in% year_trend$year) {
          df_temp <- data.frame(
            publisher = year_trend$publisher,
            journal = year_trend$journal,
            year = as.character(current_year),
            outputs = 0
          ) |>
            distinct()
          year_trend <- bind_rows(year_trend, df_temp)
        }
      }
    }

    year_trend <- year_trend |>
      arrange(year) |>
      pivot_wider(
        id_cols = c(publisher, journal),
        names_from = "year",
        values_from = "outputs"
      )

    summary_stats <- filtered_data() |>
      group_by(publisher, journal) |>
      summarise(
        total_outputs = n_distinct(doi),
        open_access_pct = 100 *
          round(sum(open_access != "closed") / total_outputs, 3),
        uob_is_corresponding_pct = 100 *
          round(sum(uob_is_corresponding) / total_outputs, 3)
      ) |>
      ungroup()

    inner_join(
      year_trend,
      summary_stats,
      by = c("publisher" = "publisher", "journal" = "journal")
    ) |>
      rename(
        Publisher = publisher,
        Journal = journal,
        `Total outputs` = total_outputs,
        `Open access (%)` = open_access_pct,
        `Corresponding authors (%)` = uob_is_corresponding_pct
      )
  })

  output$data_tbl <- renderDT(server = FALSE, {
    datatable(
      tbl_data(),
      rownames = FALSE,
      extensions = "Buttons",
      options = list(
        columnDefs = list(list(className = "dt-left", targets = 0:1)),
        dom = "Bfrtip",
        buttons = c("csv", "excel")
      )
    ) |>
      formatRound(
        columns = c('Open access (%)', 'Corresponding authors (%)'),
        digits = 1
      )
  })
}

shinyApp(ui, server)
