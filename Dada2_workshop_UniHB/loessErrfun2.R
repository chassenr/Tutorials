loessErrfun2 <- function (trans)
{
    qq <- as.numeric(colnames(trans))
    est <- matrix(0, nrow = 0, ncol = length(qq))
    for (nti in c("A", "C", "G", "T")) {
        for (ntj in c("A", "C", "G", "T")) {
            if (nti != ntj) {
                errs <- trans[paste0(nti, "2", ntj), ]
                tot <- colSums(trans[paste0(nti, "2", c("A",
                  "C", "G", "T")), ])
                rlogp <- log10((errs + 1)/tot)
                rlogp[is.infinite(rlogp)] <- NA
                df <- data.frame(q = qq, errs = errs, tot = tot,
                  rlogp = rlogp)
                mod.lo <- loess(rlogp ~ q, df, weights = log10(tot), span = 2)
                pred <- predict(mod.lo, qq)
                maxrli <- max(which(!is.na(pred)))
                minrli <- min(which(!is.na(pred)))
                pred[seq_along(pred) > maxrli] <- pred[[maxrli]]
                pred[seq_along(pred) < minrli] <- pred[[minrli]]
                est <- rbind(est, 10^pred)
            }
        }
    }
    MAX_ERROR_RATE <- 0.25
    MIN_ERROR_RATE <- 1e-07
    est[est > MAX_ERROR_RATE] <- MAX_ERROR_RATE
    est[est < MIN_ERROR_RATE] <- MIN_ERROR_RATE
    err <- rbind(1 - colSums(est[1:3, ]), est[1:3, ], est[4,
        ], 1 - colSums(est[4:6, ]), est[5:6, ], est[7:8, ], 1 -
        colSums(est[7:9, ]), est[9, ], est[10:12, ], 1 - colSums(est[10:12,
        ]))
    rownames(err) <- paste0(rep(c("A", "C", "G", "T"), each = 4),
        "2", c("A", "C", "G", "T"))
    colnames(err) <- colnames(trans)
    return(err)
}
