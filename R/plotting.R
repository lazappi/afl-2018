plot_predictions <- function(predictions, round, palettes) {
    plot_data <- predictions %>%
        mutate(Positive = Margin >= 0) %>%
        mutate(Winner = if_else(Positive, Home, Away)) %>%
        mutate(Offset = -sign(Margin) * 1.7) %>%
        mutate(HomePct = round(Result * 100),
               AwayPct = 100 - HomePct) %>%
        mutate(HomeLabel = paste0(Home, " ", HomePct, "%"),
               AwayLabel = paste0(AwayPct, "%", " ", Away)) %>%
        mutate(Game = n():1)

    fill <- palettes$primary[plot_data$Winner]
    colour <- palettes$secondary[plot_data$Winner]
    text <- palettes$text[plot_data$Winner]
    labels <- palettes$on_white[rev(plot_data$Home)]
    labels_right <- palettes$on_white[rev(plot_data$Away)]

    ggplot(plot_data,
           aes(x = Game, y = -Margin, fill = Winner, colour = Winner)) +
        geom_col(size = 2, width = 0.8,
                 position = position_dodge(width = 0.9)) +
        geom_hline(yintercept = 0, size = 2) +
        geom_text(aes(y = -(Margin + Offset),  label = abs(round(Margin))),
                  colour = text, size = 6) +
        scale_x_continuous(breaks = seq_len(nrow(plot_data)),
                           labels = rev(plot_data$HomeLabel),
                           sec.axis = sec_axis(~.,
                                               breaks = seq_len(nrow(plot_data)),
                                               labels = rev(plot_data$AwayLabel)),
                           expand = c(0,0.6)) +
        scale_fill_manual(values = fill) +
        scale_colour_manual(values = colour) +
        ylim(-max(abs(plot_data$Margin)), max(abs(plot_data$Margin))) +
        coord_flip() +
        labs(title = (paste("Predictions -", round)),
             subtitle = "Bars show estimated margins")+
        theme_minimal() +
        theme(legend.position = "none",
              plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
              plot.subtitle = element_text(size = 18, hjust = 0.5),
              axis.title = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_text(size = 14, face = "bold",
                                         colour = labels),
              axis.text.y.right = element_text(colour = labels_right),
              panel.grid = element_blank())
}


plot_results <- function(model, palettes) {

    round <- model$round

    season <- model$match_history %>%
        filter(Season == 2018)

    correct <- sum(sign(season$Margin) == sign(season$Predicted))
    accuracy <-  correct / nrow(season) * 100

    mae <- mean(abs(season$Margin - season$Predicted))

    plot_data <- season %>%
        filter(Round == round) %>%
        mutate(HomeTeam = as.character(HomeTeam),
               AwayTeam = as.character(AwayTeam)) %>%
        mutate(Positive = Margin >= Predicted) %>%
        mutate(PredWinner = if_else(Predicted > 0, HomeTeam, AwayTeam)) %>%
        mutate(RealWinner = if_else(Margin > 0, HomeTeam, AwayTeam)) %>%
        mutate(Offset = if_else(Positive, 2, -2)) %>%
        mutate(HomeLabel = HomeTeam,
               AwayLabel = AwayTeam) %>%
        mutate(Game = n():1)

    text_pred <- palettes$on_white[plot_data$PredWinner]
    text_real <- palettes$on_white[plot_data$RealWinner]
    labels <- palettes$on_white[rev(plot_data$HomeTeam)]
    labels_right <- palettes$on_white[rev(plot_data$AwayTeam)]

    ggplot(plot_data,
           aes(x = Game)) +
        geom_segment(aes(y = -Predicted, yend = -Margin, xend = Game),
                     arrow = arrow(length = unit(0.03, "npc")),
                     size = 1) +
        geom_hline(yintercept = 0, size = 2) +
        geom_text(aes(y = -(Predicted - Offset),  label = abs(round(Predicted))),
                   colour = text_pred, size = 6) +
        geom_text(aes(y = -(Margin + Offset),  label = abs(round(Margin))),
                  colour = text_real, size = 6) +
        scale_x_continuous(breaks = seq_len(nrow(plot_data)),
                            labels = rev(plot_data$HomeLabel),
                            sec.axis = sec_axis(~.,
                                                breaks = seq_len(nrow(plot_data)),
                                                labels = rev(plot_data$AwayLabel)),
                            expand = c(0,0.6)) +
        scale_colour_manual(values = colour) +
        ylim(-max(abs(plot_data$Predicted) + 2, abs(plot_data$Margin) + 2),
              max(abs(plot_data$Predicted) + 2, abs(plot_data$Margin) + 2)) +
        coord_flip() +
        labs(title = (paste("Results -", round)),
             subtitle = "Difference between estimated and actual margins",
             y = paste("Season accuracy", paste0(round(accuracy), "%"),
                       paste0("(", correct, "/", nrow(season), "),"),
                       "Margin MAE", round(mae, 2), "pts")) +
        theme_minimal() +
        theme(legend.position = "none",
              plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
              plot.subtitle = element_text(size = 18, hjust = 0.5),
              axis.title.y = element_blank(),
              axis.text.x = element_blank(),
              axis.text.y = element_text(size = 14, face = "bold",
                                         colour = labels),
              axis.text.y.right = element_text(colour = labels_right),
              panel.grid = element_blank())
}


plot_ladder <- function(sim_summ, round, palettes) {
    plot_data <- sim_summ %>%
        select(Team, starts_with("Ladder")) %>%
        arrange( -Ladder1,  -Ladder2,  -Ladder3,  -Ladder4,  -Ladder5,
                 -Ladder6,  -Ladder7,  -Ladder8,  -Ladder9, -Ladder10,
                -Ladder11, -Ladder12, -Ladder13, -Ladder14, -Ladder15,
                -Ladder16, -Ladder17, -Ladder18) %>%
        mutate(Team = factor(Team, levels = rev(Team))) %>%
        gather("Position", "Percentage", -Team) %>%
        mutate(Position = as.numeric(str_remove(Position, "Ladder"))) %>%
        mutate(Label = round(Percentage * 100),
               Label = if_else(Label >= 1, as.character(Label), "")) %>%
        mutate(Percentage = if_else(Percentage > 0,
                                    Percentage, NA_real_))

    labels <- palettes$on_white[levels(plot_data$Team)]

    ggplot(plot_data, aes(x = Position, y = Team, fill = Percentage)) +
        geom_tile() +
        geom_text(aes(label = Label)) +
        scale_x_continuous(breaks = 1:18) +
        scale_fill_viridis(option = "magma", begin = 0.3, direction = -1,
                           na.value = "white") +
        ggtitle("Simulated ladder") +
        labs(title = paste("Simulated ladder -", round),
             subtitle = "Percentage probability of finishing in each position",
             x = "Ladder position") +
        theme_minimal() +
        theme(legend.position = "none",
              panel.grid = element_blank(),
              plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
              plot.subtitle = element_text(size = 18, hjust = 0.5),
              axis.title = element_text(size = 18, face = "bold"),
              axis.title.y = element_blank(),
              axis.text = element_text(size = 14, face = "bold"),
              axis.text.y = element_text(colour = labels),
              axis.ticks.length = unit(0, "lines"))
}


plot_points <- function(sim_summ, round, palettes) {
    plot_data <- sim_summ %>%
        select(Team, Points = PointsMedian, Lower = PointsLower,
               Upper = PointsUpper) %>%
        arrange(desc(Points), desc(Upper), desc(Lower)) %>%
        mutate(Team = factor(Team, levels = rev(Team)))

    labels <- palettes$on_white[levels(plot_data$Team)]

    ggplot(plot_data, aes(x = Points, y = Team)) +
        geom_errorbarh(aes(xmin = Lower, xmax = Upper),
                       height = 0, size = 2, colour = rev(labels)) +
        geom_point(size = 5, colour = rev(labels)) +
        scale_x_continuous(breaks = seq(0, 92, 4)) +
        labs(title = paste("Estimated premiership points -", round),
             subtitle = "Teams are 75% likely to fall somewhere on the line",
             x = "Premiership points") +
        theme_minimal() +
        theme(legend.position = "none",
              panel.grid = element_blank(),
              plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
              plot.subtitle = element_text(size = 18, hjust = 0.5),
              axis.title = element_text(size = 18, face = "bold"),
              axis.title.y = element_blank(),
              axis.text = element_text(size = 14, face = "bold"),
              axis.text.y = element_text(colour = labels),
              axis.ticks.length = unit(0, "lines"))
}

plot_history <- function(model, round, palettes) {
    plot_data <- model$rating_history %>%
        data.frame() %>%
        rownames_to_column("Team") %>%
        select(Team, contains("2018")) %>%
        mutate(!!paste0(model$round, "2018") := model$ratings$Rating) %>%
        gather("Round", "Rating", -Team) %>%
        mutate(Round = str_remove(Round, "2018")) %>%
        mutate(Round = if_else(Round == "Preseason", "0", Round)) %>%
        mutate(Round = str_remove(Round, "R")) %>%
        mutate(Round = as.numeric(Round))

    ggplot(plot_data, aes(x = Round, y = Rating, colour = Team)) +
        geom_hline(yintercept = 1500) +
        geom_line(size = 2) +
        geom_point(size = 3) +
        facet_wrap(~ Team, ncol = 3) +
        scale_x_continuous(breaks = seq(5, 23, 5), limits = c(0, 23)) +
        scale_colour_manual(values = palettes$on_white) +
        labs(title = paste("Ratings history"),
             subtitle = paste("Ratings for teams to the end of", round),
             x = "Round",
             y = "Rating") +
        theme_minimal() +
        theme(legend.position = "none",
              panel.grid.minor = element_blank(),
              plot.title = element_text(size = 24, face = "bold", hjust = 0.5),
              plot.subtitle = element_text(size = 18, hjust = 0.5),
              strip.text = element_text(size = 18),
              axis.title = element_text(size = 18, face = "bold"),
              axis.text = element_text(size = 14, face = "bold"),
              axis.ticks.length = unit(0, "lines"))
}
