library("ggplot2")
require(mgcv)
# p1 <- ggplot(LIZ2.sub, aes(CumOil, WCT))
# p2 <- ggplot(LIZ2.sub,  aes(Date_m, QWM))
# p1 + geom_point(aes(colour = factor(Well_N))) + geom_smooth(method = lm, )
lm_eqn = function(m) {

     l <- list(a = format(coef(m)[1], digits = 2),
               b = format(abs(coef(m)[2]), digits = 2),
               r2 = format(summary(m)$r.squared, digits = 3));

     if (coef(m)[2] >= 0)  {
          eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
     } else {
          eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
     }

     as.character(as.expression(eq));
}
# lm_eqn = function(df){
#      m = lm(y ~ x, df);
#      eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,
#                       list(a = format(coef(m)[1], digits = 2),
#                            b = format(coef(m)[2], digits = 2),
#                            r2 = format(summary(m)$r.squared, digits = 3)))
#      as.character(as.expression(eq));
# }

par(mfrow = c(2, 2))
qplot(CumOil, WCT, data = LIZ1.sub, colour = factor(Well_N), log = "x") +
     ylim(0, 100) +
     #      stat_smooth(method = "lm", formula = y ~ x, size = 1, se = TRUE, colour = "black") +
     #      stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, se = TRUE, colour = "blue") +
     #      stat_smooth(method = "loess", formula = y ~ x, size = 1, se = TRUE, colour = "red") +
     #      stat_smooth(method = "gam", formula = y ~ s(x), size = 1, se = TRUE, colour = "green") +
     #      stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, se = TRUE, colour = "violet")
     stat_smooth(aes(ymin = 0, ymax = 100), method = gam, formula = y ~ s(x)) +
     #           geom_text(aes(x = 25, y = 300, label = lm_eqn(LIZ2.sub)), parse = TRUE)
     geom_text(aes(x = 3000, y = 50, label = lm_eqn(lm(WCT ~ CumOil, LIZ1.sub))), parse = TRUE)

qplot(CumOil, WCT, data = LIZ2.sub, colour = factor(Well_N), log = "x") +
          ylim(0, 100) +
#      stat_smooth(method = "lm", formula = y ~ x, size = 1, se = TRUE, colour = "black") +
#      stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, se = TRUE, colour = "blue") +
#      stat_smooth(method = "loess", formula = y ~ x, size = 1, se = TRUE, colour = "red") +
#      stat_smooth(method = "gam", formula = y ~ s(x), size = 1, se = TRUE, colour = "green") +
#      stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, se = TRUE, colour = "violet")
          stat_smooth(aes(ymin = 0, ymax = 100), method = gam, formula = y ~ s(x)) +
#           geom_text(aes(x = 25, y = 300, label = lm_eqn(LIZ2.sub)), parse = TRUE)
          geom_text(aes(x = 3000, y = 50, label = lm_eqn(lm(WCT ~ CumOil, LIZ2.sub))), parse = TRUE)

qplot(CumOil, WCT, data = LIZ2.sub, colour = factor(Well_N), log = "x") +
     ylim(0, 100) +
          stat_smooth(method = "lm", formula = y ~ x, size = 1, se = TRUE, colour = "black") +
#           stat_smooth(method = "lm", formula = y ~ x + I(x^2), size = 1, se = TRUE, colour = "blue") +
#           stat_smooth(method = "loess", formula = y ~ x, size = 1, se = TRUE, colour = "red") +
     #      stat_smooth(method = "gam", formula = y ~ s(x), size = 1, se = TRUE, colour = "green") +
     #      stat_smooth(method = "gam", formula = y ~ s(x, k = 3), size = 1, se = TRUE, colour = "violet")
#      stat_smooth(aes(ymin = 0, ymax = 100), method = gam, formula = y ~ s(x)) +
     #           geom_text(aes(x = 25, y = 300, label = lm_eqn(LIZ2.sub)), parse = TRUE)
     geom_text(aes(x = 3000, y = 50, label = lm_eqn(lm(WCT ~ CumOil, LIZ2.sub))), parse = TRUE)

