# compute p2 given p1 and OR
p1 <- c(rep(0.1, 3), rep(0.15, 3))
OR <- rep(c(2, 2.5, 3), 2)

odds2 <- p1 / (1 - p1)

p2 <- odds2 * OR / (1 + odds2 * OR)
p2

RR <- p2 / p1

res <- data.frame(rbind(disp(p1, 2), disp(OR, 1), disp(p2, 3)))
rownames(res) <- c("prob control group", "odds ratio", "prob tmt group")
res