# Originally based on https://github.com/richarddmorey/jpower

test1VarClass <- R6::R6Class(
  "test1VarClass",
  inherit = tTestBaseClass,
  private = list(

    #### Compute results ----
    .compute = function(stats) {
      ## Compute numbers for table
      pow.n <- NULL
      pow.es <- NULL
      pow.pow <- NULL
      if(self$options$calc == "n")
        pow.n <- ceiling(pwr.var.test(rho = stats$es, sig.level = stats$alpha, power = stats$pow, alternative = stats$alt)$n)
      if(self$options$calc == "es")
        pow.es <- pwr.var.test(n = stats$n, power = stats$pow, sig.level = stats$alpha, alternative = stats$alt)$rho
      if(self$options$calc == "power")
        pow.pow <- pwr.var.test(n = stats$n, rho = stats$es, sig.level = stats$alpha, alternative = stats$alt)$power

      return(list(n = pow.n, es = pow.es, power = pow.pow))
    },

    #### Init table ----
    .initPowerTab = function(results, stats) {
      table <- self$jaspResults[["powertab"]]
      if (is.null(table)) {
        # Create table if it doesn't exist yet
        table <- createJaspTable(title = gettext("A Priori Power Analysis"))
        table$dependOn(c(
          "test",
          "es",
          "power",
          "n",
          "alt",
          "alpha",
          "calc",
          "n_ratio"
        ))
        table$position <- 2
        self$jaspResults[["powertab"]] <- table
      } else {
        return()
      }

      calc <- self$options$calc

      if (calc == "n") {
        order <- c(1, 2, 3, 4)
      } else if (calc == "es") {
        order <- c(2, 1, 3, 4)
      } else if (calc == "power") {
        order <- c(3, 1, 2, 4)
      } else {
        order <- c(4, 1, 2, 3)
      }

      colNames <- c("n", "es", "power", "alpha")
      colLabels <- c(
        "N",
        gettext("Variance ratio (\u03C1)"),
        gettext("Power"),
        "\u03B1"
      )
      colType <- c("integer", "number", "number", "number")

      for (i in seq_along(order)) {
        table$addColumnInfo(colNames[order[i]],
                            title = colLabels[order[i]],
                            overtitle = if (i > 1) gettext("User Defined") else NULL,
                            type = colType[order[i]]
        )
      }

      row <- list()
      for (i in 2:4) {
        row[[colNames[order[i]]]] <- self$options[[colNames[order[i]]]]
      }

      table$addRows(rowNames = 1, row)

      private$.populatePowerTab(results, stats)
    },
    .initPowerESTab = function(results, stats) {
      table <- self$jaspResults[["powerEStab"]]
      if (is.null(table)) {
        # Create table if it doesn't exist yet
        table <- createJaspTable(title = gettext("Power by variance ratio"))
        table$dependOn(c(
          "test",
          "es",
          "power",
          "n",
          "alt",
          "alpha",
          "calc",
          "n_ratio",
          "text"
        ))
        table$position <- 4
        self$jaspResults[["powerEStab"]] <- table
      } else {
        return()
      }

      table$addColumnInfo(
        name = "es",
        title = gettext("True variance ratio (\u03C1)"),
        type = "string"
      )
      table$addColumnInfo(
        name = "power",
        title = gettext("Power to detect"),
        type = "string"
      )
      table$addColumnInfo(
        name = "desc",
        title = gettext("Description"),
        type = "string"
      )

      pow <- c("\u226450%", "50% \u2013 80%", "80% \u2013 95%", "\u226595%")
      desc <- c(
        gettext("Likely miss"),
        gettext("Good chance of missing"),
        gettext("Probably detect"),
        gettext("Almost surely detect")
      )

      for (i in 1:4) {
        row <- list("power" = pow[i], "desc" = desc[i])
        table$addRows(rowNames = i, row)
      }

      private$.populatePowerESTab(results, stats)
    },

    #### Populate texts ----
    .populateContourText = function(r, lst) {
      html <- self$jaspResults[["contourText"]]
      if (is.null(html)) {
        html <- createJaspHtml()
        html$dependOn(c("test", "text", "powerContour"))
        html$position <- 6
        self$jaspResults[["contourText"]] <- html
      }

      str <- gettextf(
        "<p>The power contour plot shows how the sensitivity of the test changes with the hypothetical variance ratio and the sample sizes in the design. As we increase the sample sizes, smaller variance ratios become reliably detectable.<p>Conversely, if one is satisfied to reliably detect only larger variance ratios, smaller sample sizes are needed. The point shows the power of specified  design and variance ratio."
      )

      html[["text"]] <- str
    },
    .populatePowerCurveESText = function(r, lst) {
      html <- self$jaspResults[["curveESText"]]
      if (is.null(html)) {
        html <- createJaspHtml()
        html$dependOn(c("test", "text", "powerCurveES"))
        html$position <- 8
        self$jaspResults[["curveESText"]] <- html
      }

      ## Get options from interface
      calc <- self$options$calc
      n <- ifelse(calc == "n", r$n, lst$n)
      if(calc == "es") d <- ifelse(self$options$directionOfEffect == "greater", r$es[1], r$es[2]) else d <- lst$es
      power <- ifelse(calc == "power", r$power, lst$pow)
      alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
      alt <- lst$alt
      if (alt == "two.sided" && d < 1) {
        d <- ifelse(calc == "es",
                    d,
                    ifelse(calc == "n",
                           try(pwr.var.test(n = n, power = power, sig.level = alpha, alternative = alt)$rho[2]),
                           lst$es))
      } else {
        d <- ifelse(calc == "es",
                    d,
                    ifelse(calc == "n",
                           try(pwr.var.test(n = n, power = power, sig.level = alpha, alternative = alt)$rho[1]),
                           lst$es))
      }
      if(inherits(d, "try-error")) {
        return()
      } else{
        d <- round(as.numeric(d), 3)
      }

      n_text <- gettextf("sample sizes of %s", n)

      if (d >1) {
        alt_text <- "<i>\u03C1\u003E</i>"
      } else {
        alt_text <- "<i>\u03C1\u003C</i>"

      }

      if (calc == "power") {
        pwr_string <- gettextf("have power of at least %s", round(power, 3))
      } else {
        pwr_string <- gettextf("only be sufficiently sensitive (power > %s)", round(power, 3))
      }

      if (alt == "two.sided" && d < 1) {
        d50 <- try(pwr.var.test(n = n, sig.level = alpha, power = .5, alternative = alt)$rho[2])
        if (inherits(d50, "try-error"))
          return()
        interval <- gettextf("1 > %s > %s", "\u03C1", round(d50, 3))
      } else {
        d50 <- try(pwr.var.test(n = n, sig.level = alpha, power = .5, alternative = alt)$rho[1])
        if (inherits(d50, "try-error"))
          return()
        if (alt == "less") {
          interval <- gettextf("1 > %s > %s", "\u03C1", round(d50, 3))
        } else {
          interval <- gettextf("1 < %s < %s", "\u03C1", round(d50, 3))
        }

      }

      str <- gettextf(
        "<p>The power curve above shows how the sensitivity of the test and design is larger for larger variance ratios. If we obtained %s our test and design would %s to variance ratios of %s%s. <p>We would be more than likely to miss (power less than 50%%) variance ratios of %s.",
        n_text, pwr_string, alt_text, d, interval
      )

      html[["text"]] <- str
    },
    .populatePowerCurveNText = function(r, lst) {
      html <- self$jaspResults[["curveNText"]]
      if (is.null(html)) {
        html <- createJaspHtml()
        html$dependOn(c("test", "text", "powerCurveN"))
        html$position <- 10
        self$jaspResults[["curveNText"]] <- html
      }

      ## Get options from interface
      calc <- self$options$calc
      n <- ifelse(calc == "n", r$n, lst$n)
      if(calc == "es") d <- ifelse(self$options$directionOfEffect == "greater", r$es[1], r$es[2]) else d <- lst$es
      d <- round(d, 3)
      power <- ifelse(calc == "power", r$power, lst$pow)
      alt <- lst$alt

      n_text <- gettextf("sample sizes of at least %s", n)

      if(alt == "two.sided") {
        alt_text <- "<i>\u03C1\u2260</i>1"
      } else {
        if (d >1) {
          alt_text <- "<i>\u03C1\u003E</i>1"
        } else {
          alt_text <- "<i>\u03C1\u003C</i>1"
        }
      }

      str <- gettextf(
        "<p>The power curve above shows how the sensitivity of the test and design is larger for larger sample sizes. In order for our test and design to have sufficient sensitivity (power > %s) to detect that %s when the variance ratio is %s or more extreme, we would need %s.",
        round(power, 3), alt_text, d, n_text
      )

      html[["text"]] <- str
    },
    .populateDistText = function(r, lst) {
      html <- self$jaspResults[["distText"]]
      if (is.null(html)) {
        html <- createJaspHtml()
        html$dependOn(c("test", "text", "powerDist"))
        html$position <- 12
        self$jaspResults[["distText"]] <- html
      }

      ## Get options from interface
      calc <- self$options$calc
      n <- ifelse(calc == "n", r$n, lst$n)
      if(calc == "es") d <- ifelse(self$options$directionOfEffect == "greater", r$es[1], r$es[2]) else d <- lst$es
      d <- round(d, 2)
      alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
      alt <- lst$alt
      power <- ifelse(calc == "power",
                      r$power,
                      ifelse(calc == "n",
                             pwr.var.test(n = n, rho = d, sig.level = alpha, alternative = alt)$power,
                             lst$pow))

      n_text <- gettextf("a sample size of %s", n)

      if (alt == "two.sided") {
        tail_text <- gettext("two-sided")
        null_text <- gettext("<i>\u03C1=</i>1,")
        crit_text <- gettext("criteria")
        hypo_text <- gettextf("%s%s", "\u03C1", "\u2260")
        if (d > 1)
          alt_text <- "<i>\u03C1\u2265</i>"
        if (d < 1)
          alt_text <- "<i>\u03C1\u2264</i>"
      } else {
        tail_text <- gettext("one-sided")
        crit_text <- gettext("criterion")
        if (d > 1){
          alt_text <- "<i>\u03C1\u2265</i>"
          hypo_text <- gettextf("%s>", "\u03C1")
          null_text <- "<i>\u03C1\u2264</i>1,"
        }
        if (d < 1) {
          alt_text <- "<i>\u03C1\u2264</i>"
          hypo_text <- gettextf("%s<", "\u03C1")
          null_text <- "<i>\u03C1\u2265</i>1,"
        }
      }

      str <- paste(
        "<p>",
        gettextf("The figure above shows two sampling distributions: the sampling distribution of the <i>estimated</i> variance ratio when <i>%s=</i>1, and when <i>%s=</i>%s.", "\u03C1", "\u03C1", d),
        gettextf("Both assume %s.", n_text),
        "</p><p>",
        gettextf("The vertical dashed lines show the %s we would set for a %s test with <i>α=</i>%s.", crit_text, tail_text, alpha),
        gettextf("When the observed variance ratio is far enough away from 1 to be more extreme than the %s we say we 'reject' the null hypothesis.", crit_text),
        gettextf("If the null hypothesis were true and %s the evidence would lead us to wrongly reject the null hypothesis at most %s%% of the time.", null_text, 100 * alpha),
        "</p><p>",
        gettextf("On the other hand, if <i>%s</i>%s, the evidence would exceed the criterion  &mdash; and hence we would correctly claim that <i>%s</i>1 &mdash; at least %s%% of the time.", alt_text, d, hypo_text, 100 * round(power, 3)),
        gettextf("The design's power for detecting effects of %s%s is thus %s.", alt_text, d, round(power, 3)),
        "</p>"
      )


      html[["text"]] <- str
    },

    #### Populate table ----
    .populatePowerTab = function(r, lst) {
      table <- self$jaspResults[["powertab"]]

      calc <- self$options$calc
      n <- ifelse(calc == "n", r$n, lst$n)
      if(calc == "es") d <- ifelse(self$options$directionOfEffect == "greater", r$es[1], r$es[2]) else d <- lst$es
      power <- ifelse(calc == "power", r$power, lst$pow)
      alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
      alt <- lst$alt

      if (calc == "es") {
        row <- list()
        row[[calc]] <- d
        table$addColumns(row)
      } else {
        row <- list()
        row[[calc]] <- r[[calc]]
        table$addColumns(row)
      }

      if (calc == "n") {
        if (round(pwr.var.test(n = n, rho = d, sig.level = alpha, alternative = alt)$power, 3) == 1) {
          table$addFootnote(gettextf("Due to the rounding of the sample size, the actual power can deviate from the target power. <b>Actual power: >0.999"))
        } else {
          table$addFootnote(gettextf("Due to the rounding of the sample size, the actual power can deviate from the target power. <b>Actual power: %1$s</b>",
                                     round(pwr.var.test(n = n, rho = d, sig.level = alpha, alternative = alt)$power, 3)
          ))
        }
      }
    },
    .populatePowerESTab = function(r, lst) {
      html <- self$jaspResults[["tabText"]]
      if (is.null(html)) {
        html <- createJaspHtml()
        html$dependOn(c("test", "text"))
        html$position <- 3
        self$jaspResults[["tabText"]] <- html
      }

      ## Get options from interface
      calc <- self$options$calc
      n <- ifelse(calc == "n", r$n, lst$n)
      if(calc == "es") d <- ifelse(self$options$directionOfEffect == "greater", r$es[1], r$es[2]) else d <- lst$es
      power <- ifelse(calc == "power", r$power, lst$pow)
      alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
      alt <- lst$alt

      n_text <- gettextf("a sample size of ", n)

      tail_text <- ifelse(alt == "two.sided",
                          gettext("two-sided"),
                          gettext("one-sided")
      )

      sign <- ifelse(d > 1, "\u2265", "\u2264")


      if (calc == "n") {
        str <- gettextf(
          "We would need %s to reliably (with probability greater than %s) detect a variance ratio of <i>%s%s</i>%s, assuming a %s criterion for detection that allows for a maximum Type I error rate of <i>α=</i>%s.",
          n_text, power, "\u03C1", sign, d, tail_text, alpha
        )
      } else if (calc == "es") {
        str <- gettextf(
          "A design with %s will reliably (with probability greater than %s) detect variance ratios of <i>%s%s</i>%s, assuming a %s criterion for detection that allows for a maximum Type I error rate of <i>α=</i>%s.",
          n_text, power, "\u03C1", sign, round(d, 3), tail_text, alpha
        )
      } else if (calc == "power") {
        str <- gettextf(
          "A design with %s can detect variance ratios of %s<i>%s</i>%s with a probability of at least %s, assuming a %s criterion for detection that allows for a maximum Type I error rate of <i>α=</i>%s.",
          n_text, "\u03C1", sign, round(d, 3), round(power, 3), tail_text, alpha
        )
      }

      hypo_text <- ifelse(alt == "two.sided",
                          gettextf("<i>%s</i>%s1", "\u03C1", "\u2260"),
                          ifelse(alt == "less", "<i>\u03C1<1</i>", "<i>\u03C1>1</i>")
      )

      str <- paste0(
        str,
        gettextf(
          "<p>To evaluate the design specified in the table, we can consider how sensitive it is to true effects of increasing sizes; that is, are we likely to correctly conclude that %s when the variance ratio is large enough to care about?",
          hypo_text
        )
      )

      html[["text"]] <- str

      table <- self$jaspResults[["powerEStab"]]

      probs <- c(.5, .8, .95)
      probs_es <- try(sapply(probs, function(p) {
        if(alt == "two.sided" && d < 1) {
          pwr.var.test(
            n = n, sig.level = alpha, power = p,
            alternative = alt)$rho[2]
        } else {
          pwr.var.test(
            n = n, sig.level = alpha, power = p,
            alternative = alt)$rho[1]
        }
      }))
      if(inherits(probs_es, "try-error")) {
        table$setError(gettext("The specified design leads to (an) unsolvable equation(s) while computing the values for this power table. Try to enter less extreme values for the parameters."))
        return()
      }
      sign1 <- ifelse(alt == "less" || (alt == "two.sided" && d < 1), ">", "<")
      sign2 <- ifelse(alt == "less" || (alt == "two.sided" && d < 1), "\u2265", "\u2264")
      sign3 <- ifelse(alt == "less" || (alt == "two.sided" && d < 1), "\u2264", "\u2265")

      esText <- c(
        gettextf("1 %s %s %s  %s", sign1, "\u03C1", sign2, format(round(probs_es[1], 3), nsmall = 3)),
        gettextf("%s %s %s %s %s", format(round(probs_es[1], 3), nsmall = 3), sign1, "\u03C1", sign2, format(round(probs_es[2], 3), nsmall = 3)),
        gettextf("%s %s %s %s %s",format(round(probs_es[2], 3), nsmall = 3), sign1, "\u03C1", sign2, format(round(probs_es[3], 3), nsmall = 3)),
        gettextf("%s %s %s", "\u03C1", sign3, format(round(probs_es[3], 3), nsmall = 3))
      )

      cols <- list("es" = esText)
      table$addColumns(cols)
      if(alt == "two.sided" && calc != "es")
        table$addFootnote(gettext("To improve readability of this power table for a 'Two-sided' alternative hypothesis, only the intervals in the direction of the specified variance ratio are displayed under 'True variance ratio'"))
    },

    #### Plot functions ----
    .preparePowerContour = function(r, lst) {
      image <- self$jaspResults[["powerContour"]]
      if (is.null(image)) {
        image <- createJaspPlot(title=gettext("Power Contour"), width=400, height=350)
        image$dependOn(c(
          "test",
          "es",
          "power",
          "n",
          "alt",
          "alpha",
          "calc",
          "n_ratio",
          "powerContour"
        ))
        image$position <- 5
        self$jaspResults[["powerContour"]] <- image
      }

      calc <- self$options$calc
      n <- ifelse(calc == "n", r$n, lst$n)
      if(calc == "es") d <- ifelse(self$options$directionOfEffect == "greater", r$es[1], r$es[2]) else d <- lst$es
      alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
      alt <- lst$alt
      power <- ifelse(calc == "power",
                      r$power,
                      ifelse(calc == "n",
                             pwr.var.test(n = n, rho = d, sig.level = alpha, alternative = alt)$power,
                             lst$pow))

      ps <- ttestPlotSettings

      maxn <- ceiling(pwr.var.test(
        rho = d, power = max(0.99, power), sig.level = alpha, alternative = alt)$n)

      if (n >= maxn && n >= ps$maxn) {
        maxn <- ceiling(n * ps$max.scale)
      } else if (maxn < ps$maxn) {
        if ((ps$maxn - n) < 20) {
          maxn <- ps$maxn * ps$max.scale
        } else {
          maxn <- ps$maxn
        }
      }

      minn <- 2
      try <- try(pwr.var.test(n = minn, sig.level = alpha, power = power, alternative = alt))
      while (inherits(try, "try-error")) {
        minn <- minn + 1
        try <- try(pwr.var.test(n = minn, sig.level = alpha, power = power, alternative = alt))
      }

      nn <- unique(ceiling(exp(seq(log(minn), log(maxn), len = ps$lens)) - .001))

      mind <- ifelse(alt == "two.sided" || alt == "less", min(d, 0.1), 1)
      maxd <- ifelse(alt == "less", 1,
                     ifelse(alt == "two.sided" && d < 1, max(2, ((1/d) * 1.2)), max(2, (d * 1.2)))
      )

      dd <- seq(mind, maxd, len = 20)

      z.pwr <- try(sapply(dd, function(delta) {
        pwr.var.test(n = nn, rho = delta, sig.level = alpha, alternative = alt)$power
      }))
      if(inherits(z.pwr, "try-error")) {
        image$setError(gettext("The specified design leads to (an) unsolvable equation(s) while constructing the Power Contour plot. Try to enter less extreme values for the parameters"))
        return()
      }

      z.delta <- try(sapply(nn, function(N) {
        pwr.var.test(n = N, sig.level = alpha, power = power, alternative = alt)$rho
      }))
      if(inherits(z.pwr, "try-error")) {
        image$setError(gettext("The specified design leads to (an) unsolvable equation(s) while constructing the Power Contour plot. Try to enter less extreme values for the parameters"))
        return()
      }

      state = list(
        z.pwr = z.pwr,
        z.delta = z.delta,
        ps = ps,
        nn = nn,
        dd = dd,
        n = n,
        delta = d,
        alpha = alpha,
        minn = minn,
        maxn = maxn
      )
      image$plotObject <- private$.powerContour(state = state, ggtheme = pwr_plot_theme())
    },
    .preparePowerCurveES = function(r, lst) {
      image <- self$jaspResults[["powerCurveES"]]
      if (is.null(image)) {
        image <- createJaspPlot(
          title = gettext("Power Curve by variance ratio"),
          width = 400,
          height = 350
        )
        image$dependOn(c(
          "test",
          "es",
          "power",
          "n",
          "alt",
          "alpha",
          "calc",
          "n_ratio",
          "powerCurveES"
        ))
        image$position <- 7
        self$jaspResults[["powerCurveES"]] <- image
      }

      calc <- self$options$calc
      n <- ifelse(calc == "n", r$n, lst$n)
      if(calc == "es") d <- ifelse(self$options$directionOfEffect == "greater", r$es[1], r$es[2]) else d <- lst$es
      alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
      alt <- lst$alt
      power <- ifelse(calc == "power",
                      r$power,
                      ifelse(calc == "n",
                             pwr.var.test(n = n, rho = d, sig.level = alpha, alternative = alt)$power,
                             lst$pow))

      ps <- ttestPlotSettings

      maxd <- try(pwr.var.test(n = n, power = max(0.999, power), sig.level = alpha, alternative = alt)$rho)
      if (inherits(maxd, "try-error")) {
        mind <- ifelse(d < 1, d, 1)
        maxd <- ifelse(d < 1, 1, d)
      } else {
        if (alt == "two.sided") {
          mind <- min(as.numeric(maxd[2]), d)
          maxd <- max(as.numeric(maxd[1]), d)
        } else if(alt == "less") {
          mind <- min(as.numeric(maxd), d)
          maxd <- 1
        } else {
          mind <- 1
          maxd <- max(as.numeric(maxd), d)
        }
      }


      dd <- seq(mind, maxd, len = ps$curve.n)

      y <- try(pwr.var.test(n = n, rho = dd, sig.level = alpha, alternative = alt)$power)
      if(inherits(y, "try-error")) {
        image$setError(gettext("The specified design leads to (an) unsolvable equation(s) while constructing the power curve. Try to enter less extreme values for the parameters"))
        return()
      }
      cols <- ps$pal(ps$pow.n.levels)
      yrect <- seq(0, 1, 1 / ps$pow.n.levels)

      state = list(cols = cols, dd = dd, y = y, yrect = yrect, n = n, alpha = alpha, delta = d, pow = power)
      image$plotObject <- private$.powerCurveES(state = state, ggtheme = pwr_plot_theme())
    },
    .preparePowerCurveN = function(r, lst) {
      image <- self$jaspResults[["powerCurveN"]]
      if (is.null(image)) {
        image <- createJaspPlot(
          title = gettext("Power Curve by N"),
          width = 400,
          height = 350
        )
        image$dependOn(c(
          "test",
          "es",
          "power",
          "n",
          "alt",
          "alpha",
          "calc",
          "n_ratio",
          "powerCurveN"
        ))
        image$position <- 9
        self$jaspResults[["powerCurveN"]] <- image
      }

      calc <- self$options$calc

      ps <- ttestPlotSettings

      n <- ifelse(calc == "n", r$n, lst$n)
      if(calc == "es") d <- ifelse(self$options$directionOfEffect == "greater", r$es[1], r$es[2]) else d <- lst$es
      alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
      alt <- lst$alt
      power <- ifelse(calc == "power",
                      r$power,
                      ifelse(calc == "n",
                             pwr.var.test(n = n, rho = d, sig.level = alpha, alternative = alt)$power,
                             lst$pow))

      maxn <- try(ceiling(pwr.var.test(
        rho = d, power = max(0.99999, power), sig.level = alpha, alternative = alt)$n))

      if(inherits(maxn, "try-error")) {
        maxn <- n
      } else if (n >= maxn && n >= ps$maxn) {
        maxn <- ceiling(n * ps$max.scale)
      }


      minn <- 2
      try <- try(pwr.var.test(n = minn, sig.level = alpha, power = power, alternative = alt))
      while (inherits(try, "try-error")) {
        minn <- minn + 1
        try <- try(pwr.var.test(n = minn, sig.level = alpha, power = power, alternative = alt))
      }

      nn <- seq(minn, maxn)

      y <- try(pwr.var.test(n = nn, rho = d, sig.level = alpha, alternative = alt)$power)
      if(inherits(y, "try-error")) {
        image$setError(gettext("The specified design leads to (an) unsolvable equation(s) while constructing the 'Power Curve by N' plot. Try to enter less extreme values for the parameters"))
        return()
      }

      cols <- ps$pal(ps$pow.n.levels)
      yrect <- seq(0, 1, 1 / ps$pow.n.levels)

      lims <- data.frame(
        xlim = c(minn, maxn),
        ylim = c(0, 1)
      )

      state = list(
        n = n,
        cols = cols,
        nn = nn,
        y = y,
        yrect = yrect,
        lims = lims,
        delta = d,
        alpha = alpha,
        pow = power
      )
      image$plotObject <- private$.powerCurveN(state = state, ggtheme = pwr_plot_theme())
    },
    .preparePowerDist = function(r, lst) {
      image <- self$jaspResults[["powerDist"]]
      if (is.null(image)) {
        image <- createJaspPlot(
          title = gettext("Power Demonstration"),
          width = 400,
          height = 300
        )
        image$dependOn(c(
          "test",
          "es",
          "power",
          "n",
          "alt",
          "alpha",
          "calc",
          "n_ratio",
          "powerDist"
        ))
        image$position <- 11
        self$jaspResults[["powerDist"]] <- image
      }

      calc <- self$options$calc

      n <- ifelse(calc == "n", r$n, lst$n)
      if(calc == "es") d <- ifelse(self$options$directionOfEffect == "greater", r$es[1], r$es[2]) else d <- lst$es
      power <- ifelse(calc == "power", r$power, lst$pow)
      alpha <- ifelse(calc == "alpha", r$alpha, lst$alpha)
      alt <- lst$alt


      df <- n
      ncp <- n * d

      if (alt == "two.sided") {
        crit_upper <- qchisq(1 - alpha / 2, df = df) / n
        crit_lower <- qchisq(alpha/2, df = df) /n
      } else if (alt == "greater") {
        crit <- qchisq(p = 1 - alpha, df = df) / n
      } else {
        crit <- qchisq(alpha, df = df) /n
      }

      if (d > 1) {
        xlims <- c(floor(qchisq(.0001, df)), qchisq(.9999, ncp)) / n
      } else {
        xlims <- c(floor(qchisq(.0001, ncp)), qchisq(.9999^(1/d), df)) / n
      }

      if(d > 1)
        y.max <- dchisq(df - 2, df)
      if(d < 1 && d > 0.3)
        y.max <- dchisq(ncp - 2, ncp)
      if(d <= 0.3)
        y.max <- max(dchisq(ncp - 2, ncp), dchisq(df - 2, df) * 4)

      xx <- seq(xlims[1], xlims[2], len = 1000)
      yy.null <- dchisq(xx * n, df)
      yy.alt <- dchisq(xx * n, ncp)

      curves <- data.frame(
        x = rep(xx, 2),
        ymin = rep(0, length(xx) * 2),
        ymax = c(yy.null, yy.alt),
        group = rep(c("Null", "Alt"), each = length(xx))
      )

      if (alt == "two.sided") {
        rect <- data.frame(
          x1 = crit_lower, x2 = crit_upper,
          y1 = 0, y2 = y.max * 1.1
        )
      } else if (alt == "greater") {
        rect <- data.frame(
          x1 = xlims[1] - 1, x2 = crit,
          y1 = 0, y2 = y.max * 1.1
        )
      } else {
        rect <- data.frame(
          x1 = crit, x2 = xlims[2] + 1,
          y1 = 0, y2 = y.max * 1.1
        )
      }

      lims <- data.frame(
        xlim = c(xlims[1], xlims[2]),
        ylim = c(0, y.max * 1.1)
      )

      state = list(curves = curves, rect = rect, lims = lims)
      image$plotObject <- private$.powerDist(state = state, ggtheme = pwr_plot_theme())
    }
  )
)
