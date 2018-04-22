summarise_sim <- function(sim) {
    sim_df <- sim %>%
        map(function(x) {
            x %>%
                arrange(desc(Points), desc(Percentage)) %>%
                mutate(Ladder = 1:n())
        }) %>%
        bind_rows()

    lower_bound <- length(sim) * 0.125
    upper_bound <- length(sim) * 0.875

    sim_summ <- sim_df %>%
        group_by(Team) %>%
        summarise(RatingMean   = mean(Rating),
                  PointsMean   = mean(Points),
                  PointsMedian = median(Points),
                  PointsLower  = sort(Points)[lower_bound],
                  PointsUpper  = sort(Points)[upper_bound],
                  Ladder1      = sum(Ladder == 1),
                  Ladder2      = sum(Ladder == 2),
                  Ladder3      = sum(Ladder == 3),
                  Ladder4      = sum(Ladder == 4),
                  Ladder5      = sum(Ladder == 5),
                  Ladder6      = sum(Ladder == 6),
                  Ladder7      = sum(Ladder == 7),
                  Ladder8      = sum(Ladder == 8),
                  Ladder9      = sum(Ladder == 9),
                  Ladder10     = sum(Ladder == 10),
                  Ladder11     = sum(Ladder == 11),
                  Ladder12     = sum(Ladder == 12),
                  Ladder13     = sum(Ladder == 13),
                  Ladder14     = sum(Ladder == 14),
                  Ladder15     = sum(Ladder == 15),
                  Ladder16     = sum(Ladder == 16),
                  Ladder17     = sum(Ladder == 17),
                  Ladder18     = sum(Ladder == 18)) %>%
        mutate_at(vars(starts_with("Ladder")),
                  function(x) {x / length(sim)}) %>%
        mutate(Top8 = Ladder1 + Ladder2 + Ladder3 + Ladder4 +
                      Ladder5 + Ladder6 + Ladder7 + Ladder8,
               Top4 = Ladder1 + Ladder2 + Ladder3 + Ladder4,
               Top2 = Ladder1 + Ladder2) %>%
        arrange(desc(RatingMean))

    return(sim_summ)
}
