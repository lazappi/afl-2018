make_summ_table <- function(sim_summ, model) {
    history <- model$rating_history[sim_summ$Team, ]

    sim_summ %>%
        left_join(model$ratings, by = "Team") %>%
        left_join(model$ladder, by = "Team") %>%
        mutate(PrevRating = history[Team, ncol(history)]) %>%
        mutate(Change = Rating - PrevRating) %>%
        select(Team, Rating, Change, Points, Percentage,
               ProjRating = RatingMean, ProjPoints = PointsMean,
               Top2, Top4, Top8) %>%
        mutate(Rating = round(Rating),
               Change = round(Change),
               Percentage = round(Percentage),
               ProjRating = round(ProjRating),
               ProjPoints = round(ProjPoints, 1),
               Top2 = round(Top2 * 100, 1),
               Top4 = round(Top4 * 100, 1),
               Top8 = round(Top8 * 100, 1)) %>%
        arrange(desc(Rating), desc(Points), desc(Percentage)) %>%
        formattable(
            list(
                Rating = color_tile("white", "lightblue"),
                Change = formatter("span",
                                    style = ~ style(color = ifelse(Change >= 0,
                                                                   "green",
                                                                   "red"))),
                ProjRating = color_tile("white", "lightblue"),
                Top2 = color_tile("white", "lightpink"),
                Top4 = color_tile("white", "lightpink"),
                Top8 = color_tile("white", "lightpink")
            )
        ) #%>%
        #as.datatable(options = list(paging = FALSE,
        #                            bInfo = FALSE,
        #                            columnDefs = list(
        #                                list(className = 'dt-center',
        #                                     targets = 1:9))),
        #             rownames = FALSE)
}
