library(collapse)
library(echarts4r)


#get total Reinvestments
totalReinvestments <- function(xVal){
  
  return((xVal$capEx_nInput - xVal$depreciation_nInput) + xVal$cwc_nInput)
}

###Table###
pvTable <- function(name, stage, ...){
  one <- stage == "One"
  
  data<- list(...) |> 
    (\(x){
      baseCOE <- if(length(bcoe <- (x$rf_nInput + (x$beta_nInput * x$erp_nInput)))) bcoe else 0
      
      #time span depending on which stage is chosen
     timeSpan <- if(!is.null(x$tspan_nInput) && is.finite(x$tspan_nInput)){
        
       #~~~~~~~~~~~~~~~~~~~Can be re-written~~~~~~
       if(one){ 
          x$tspan_nInput <- 0
          #extra insurance to deal with seq(0) returns two values instead of 1
          if(x$tspan_nInput) seq_len(x$tspan_nInput) else 1
          
        } else {
          seq(max(1, x$tspan_nInput))
        }
        
     } else {
        1
      }
      
      #discounting number, if timeSpan > 0, seq it
      discount <- `^`((1 + x$gr_nInput), timeSpan)
      coe <- `^`((1 + baseCOE), timeSpan)


      eps <- discount * (x$net_inc_nInput / x$numShares_nInput)
      dps <- discount * (x$dividends_nInput / x$numShares_nInput)
      # reinv <- ((x$capEx_nInput - x$depreciation_nINput) - cwc_nInput)
      
      #function that alters x Variables
      alterX <- function(var, vals){
        var[vals] <- with(var, lapply(vals, \(a) eval(as.symbol(a)) * discount))
        return(var)
      }
      
      #ambiguous Numbers
      if("Dividends" %in% name){
        calcValss <- paste0(c("dividends","net_inc"), "_nInput")
        x <- alterX(x, calcValss)
        pv <- dps/coe
        #no manual entry for payout ratio, it's calculated based on this formula
        payout_Ratio <- dps/eps
        tobind <- list("EPS" = eps, "COE" = coe,"Payout Ratio" = payout_Ratio, "Present Value" = pv)
      
        } else {
          
          #FCFE
          if("m_EqRR" %in% name){
            calcValss <- paste0(c("net_inc", "capEx", "depreciation", "cwc", "debt_iss"), "_nInput")
            x <- alterX(x, calcValss)
            
            
            fcfe <- if(length(x$reinv_nInput) && is.finite(x$reinv_nInput) && x$reinv_nInput > 0) x$net_inc_nInput - (x$net_inc_nInput * x$reinv_nInput) else x$net_inc_nInput  - totalReinvestments(x) + (x$debt_iss_nInput - x$debt_paid_nInput)
            pv <- fcfe/coe
            eq_Reinv <- (1 - (fcfe/x$net_inc_nInput))
            ROE <- x$net_inc_nInput/x$book_val_nInput
            fGrowth <- ROE * eq_Reinv
            
            #variables to bind to table
            tobind <- list("EPS" = eps, "COE" = coe, "FCFE" = fcfe, "ROE" = ROE, "Expected Growth" = fGrowth,
                 "Cumulated EqRR" = eq_Reinv, "Present Value" = pv)
          }
          
          else 
          #FCFF
          {
              
            calcValss <- paste0(c("capEx", "depreciation", "cwc", "debt_iss", "ebit"), "_nInput")
            x <- alterX(x, calcValss)
            EBI <- (x$ebit_nInput * (1 - x$taxRate_nInput))
            fcff <- if(length(x$reinvR_nInput) && is.finite(x$reinvR_nInput) && x$reinvR_nInput > 0) EBI - (EBI * x$reinvR_nInput) else EBI  * (1 - x$reinvR_nInput) - totalReinvestments(x)
            total_debt <- x$debt_nInput
            debt_to_capital <- total_debt/(total_debt + x$market_cap_nInput)
            CoC <- `^`((1 + ((baseCOE * (1 - debt_to_capital)) + (x$cod_nInput * (1 - x$taxRate_nInput) * debt_to_capital))), timeSpan)
            pv <- fcff / CoC
            #terminal Value - value_of_operating_assets + x$cash_nInput - total_debt
           
            
            
            #variables to bind
            tobind <- list("COE" = coe, "Cost of Capital" = CoC, "FCFF" = fcff, "Present Value" = pv)
          }

      }
      
      x <- setNames(x, name)

      numbers <- do.call(rbind,c(x, tobind))

      dimnames(numbers) <- list(dimnames(numbers)[[1]], seq_col(numbers))

      numbers
    })()
  
  return(list(data = data, name = name))
}

#-----------------------------------------------------------------------------------------------------------------------------------------------
#function for the third stage if needed
transitionTable <- function(tabl, stage, ...){

  #
  myTabl <- tabl$data
  myNames <- tabl$name
  div <- "Dividends" %in% myNames
  fCashFlowE <- "m_EqRR" %in% myNames
  
  data <- list(...) |> 
    (\(x){
    vec <- myTabl[, fncol(myTabl)]
    t <- if(length(x$d_tspan_nInput)) x$d_tspan_nInput else 0
    timeSpan <- t

    if(stage == "Three" & is.finite(t) & !is.na(vec["Time Span"])) {
      
      changeFunc <- function(init, end, period){
          
          val <- (init + `*`(((end - init)/period), if(period) seq_len(period) else 1))

          return(if(anyNA(val) | (length(val) < 1)) 0 else val)
      }
      newCapex <- NaN
      newCwC <- NaN
      newGrowth <- changeFunc(vec["GR"], x$d_growth_nInput, timeSpan)
      newIncome <- vec["Net Income"] * cumprod(1 + newGrowth)
      shares <- vec["#Shares"]
      eps <- newIncome / shares
      coeChange <- changeFunc((myTabl["COE", ][1]) - 1, x$d_coe_nInput, timeSpan)
      coe <- vec["COE"] * cumprod(1 + coeChange)
      secondBind <- list(eps, coe)
      
      bindList <- if(div){
        
        newPayout <- changeFunc(vec["Payout Ratio"], x$d_payout_nInput, timeSpan)
        dividends <- newPayout * newIncome
        pv <- dividends/coe
        
        #arguments to bind to matrix
        firstBind <- list(dividends, newIncome, shares, x$rf_nInput, x$beta_nInput, x$erp_nInput,  newGrowth, timeSpan)
        thirdBind <- list(newPayout, pv)
        
        list(firstBind = firstBind, secondBind = secondBind, thirdBind = thirdBind)
      } else  if(fCashFlowE){
        #not necessary to project capex, so kept NA in transition period
          newMeqRR <- changeFunc(vec["m_EqRR"], x$d_eq_rr_nInput, timeSpan)
          extras <- names(getVars("extraL")$FCFE)
          fcfe <- newIncome - (newIncome * newMeqRR)
          bValue <- changeFunc(vec["Book Val"], (vec["Book Val"] + cumsum(fcfe)), timeSpan)
          roe <- newIncome / bValue
          newC_eqRR <- (1 - (fcfe/newIncome))
          newExGrowth <- roe * newC_eqRR
          pv <- fcfe/coe
          # a = list("EPS" = eps, "COE" = coe, "FCFE" = fcfe, "ROE" = ROE, "Exp Growth" = fGrowth,  "cEqRR" = eq_Reinv, "Present Value" = pv)
          
          #~~~~~~~~~~~~~~~~~~~~~~~maybe try append route
          firstBind <- list(newIncome, newCapex, newCwC,newMeqRR, shares, x$rf_nInput, x$beta_nInput, x$erp_nInput, newGrowth, timeSpan)
          thirdBind <- list(fcfe,  roe, newExGrowth, newC_eqRR, pv)
          
          list(firstBind = firstBind, secondBind = secondBind, thirdBind = thirdBind, extras = extras, default = c(list(newGrowth), as.list(vec[-1])))

      } else {
        
        newMktCap <- NaN
        newDebt <- NaN
        newCOD <- NaN
        newCash <- NaN
        extras <- names(getVars("extraL")$FCFF)
        #assumes the tax Rate doesn't change
        newTaxRate <- vec["Tax Rate"]
        newEBIT <- vec["EBIT"] * cumprod(1 + newGrowth)
        newRR <- changeFunc(vec["m_ReinvRate"], x$d_ReinvR_nInput, timeSpan)
        newEBI <- newEBIT * (1 - newTaxRate)
        fcff <- newEBI - (newEBI * newRR)
        newCOC <- changeFunc(vec["Cost of Capital"], x$d_coc_nInput, timeSpan) 
        pv <- fcff/newCOC
        
        firstBind <- list(newMktCap, newEBIT, newTaxRate, newDebt, newCOD, newCash, newRR, newCapex, newCwC, shares, x$rf_nInput, x$beta_nInput, x$erp_nInput, newGrowth, timeSpan)
        second <- list(coe, newCOC)
        thirdBind <- list(fcff, pv)
        
        list(firstBind = firstBind, secondBind = second, thirdBind = secondBind, extras = extras)
      }
      
      #if extra variables are loaded
      vals <-  if(all(bindList$extras %in% names(vec))) {
        
        #return for dividends valuation
        dividend_values <- c(bindList$firstBind, secondBind, bindList$thirdBind)
        if(div) {
          dividend_values
        } else if(fCashFlowE) {
          
          rlang::inject(c(bindList$firstBind, !!!vec[bindList$extras], secondBind, bindList$thirdBind))
        } else {
          #modified second bind, eps replaced with coc
          rlang::inject(c(bindList$firstBind, !!!vec[bindList$extras], bindList$secondBind, bindList$thirdBind))
          }

      } else {
        
        if(div){
          
          dividend_values
        
          } 
        
        else if(fCashFlowE) {
          
          if(!is.na(vec["CapEx"])) {
            meRR <- vec["m_EqRR"]
            
            #is not NA and greater than 0
            if(!is.na(meRR) & meRR) {
              c(bindList$firstBind, bindList$thirdBind)
            } else {
              c(bindList$firstBind, secondBind)
            }
            
          } else {
            bindList$default
          }

        } 
        
        else {
          
          if(!is.na(vec["CapEx"])) {
            mRR <- vec["m_ReinvRate"]
            
            #is not NA and greater than 0
            if(!is.na(mRR) & mRR) {
              c(bindList$firstBind, bindList$second, bindList$thirdBind)
            } else {
              c(bindList$firstBind, bindList$second)
            }
            
          } else {
            bindList$default
          }
        }

      }
      
      mat <- do.call(rbind, vals)
      
      
      dimnames(mat) <- list(NULL, if(t) paste0("D", seq_len(t)) else "D1")

      cbind(myTabl, mat) |>
        (\(x){
          x[] <- sapply(x, round, digits = 4)
          x
        })()
    
    } else { myTabl |> 
          (\(x){
            x[] <- sapply(x, round, digits = 4)
            x
          })()
      }
      
    })()

return(data)
  }
    

#----------------------------------------------------------------------------------------------------------------------------------------------------

#function to grab value from the table given to perpValue function
grabValue <- function(value, tabl, which = NULL){
  
  which <- if(is.null(which)) fncol(tabl) else which
  
  return(tabl[dimnames(tabl)[[1]] == value, which])
}

#-------------------------------------------------------------------------------------------------------------------------------------------------------

#present value of dividends
pvPlot <- function(table){
  
  tryCatch({table[fnrow(table), ] |> 
      stack() |>
      rev() |> 
      setNames(c("year", "PV")) |> 
      e_chart(year) |>
      e_line(PV) |>
      e_tooltip(trigger = "item")}, error = function(e) e, finally = "")

}

#helper function for other functions that need to calculate terminal value, less repetition
terminalCalculator <- function(type, stage, tabl, xVal, gr){
  
 terminalValue <- switch(stage, 
   
  "One" = {
    switch(type, 
           "DIV" = 
             { coe <- (grabValue("COE", tabl) - 1) - gr
             grabValue("Dividends", tabl)/ifelse(coe, coe, 1)
             },
           "FCFE" = 
             { tEPS <- grabValue("EPS", tabl)
             tFCFE <- if(length(xVal$reinv_nInput) && is.finite(xVal$reinv_nInput) && xVal$reinv_nInput > 0) tEPS  - (tEPS * xVal$reinv_nInput) else tEPS  - totalReinvestments(xVal) + (xVal$debt_iss_nInput - xVal$debt_paid_nInput)
             
             tFCFE/((grabValue("COE", tabl) - 1) - gr) 
             },
           "FCFF" = 
             { 
               tEBIT <- grabValue("EBIT", tabl) * (1 - grabValue("Tax Rate", tabl))
               
               tFCFF <- if(length(xVal$reinvR_nInput) && is.finite(xVal$reinvR_nInput) && xVal$reinvR_nInput > 0) tEBIT - (tEBIT * xVal$reinvR_nInput) else tEBIT - (totalReinvestments(xVal) * (1 + gr))
               (tFCFF/(grabValue("Cost of Capital", tabl) - gr)) + grabValue("Cash", tabl) - grabValue("Debt", tabl)
             })
  },
  
  switch(type,
          "DIV" = 
            {
              (((grabValue("EPS", tabl) * (1 + gr) * (xVal$t_payout_nInput))/(xVal$t_coe_nInput - gr))/grabValue("COE", tabl))
            },
          "FCFE" = 
            {
              tEPS <- grabValue("EPS", tabl) * (1 + gr)
              tEquityRR <- tEPS * xVal$t_eq_rr_nInput
              tFCFE <- tEPS - tEquityRR

              (tFCFE/(xVal$t_coe_nInput - gr))/grabValue("COE", tabl)
            },
          "FCFF" =
            {
              tEBIT <- grabValue("EBIT", tabl)
              tFCFF <- tEBIT * (1 - xVal$taxRate_nInput) * (1 + gr) * (1 - xVal$t_rr_nInput)

              (tFCFF/(xVal$t_coc_nInput - gr))/grabValue("Cost of Capital", tabl)
            }
          )
  )
    
   # if(type == "DIV"){
   #   
   #   (((grabValue("EPS", tabl) * (1 + gr) * (xVal$t_payout_nInput))/(xVal$t_coe_nInput - gr))/grabValue("COE", tabl))
   # 
   #   } else if(type == "FCFE") {
   #   
   #   tEPS <- grabValue("EPS", tabl) * (1 + gr)
   #   tEquityRR <- tEPS * xVal$t_eq_rr_nInput
   #   tFCFE <- tEPS - tEquityRR
   #   
   #   (tFCFE/(xVal$t_coe_nInput - gr))/grabValue("COE", tabl)
   #   
   #  }
 
 return(terminalValue)
}

#-------------------------------------------------------------------------------------------------------

#PV of stock in perpetuity
perpValue <- function(name, tabl, stage, ...){
  one <- stage == "One"

  pvPerShare <- sum(grabValue("Present Value",tabl, which = seq_col(tabl)))/grabValue("#Shares", tabl)
  
  list(...) |>
    (\(x){
      
      #nested if-else, four possible options between stage valuation
      
      if(one) {
        
        if("m_EqRR" %in% name){
          
          #Average return in terminal phase
          terminalValue <- terminalCalculator(type = "FCFE", stage = stage, tabl = tabl, xVal = x, gr = grabValue("GR", tabl))
          # pvPerShare + (tValue / discountRate)
          return(list(firstStage = 0, secondStage = terminalValue, total = terminalValue))
          
        } else if("EBIT" %in% name) {
          
          terminalValue <- terminalCalculator(type = "FCFF", stage = stage, tabl = tabl, xVal = x, gr = grabValue("GR", tabl))
          
          return(list(firstStage = 0, secondStage = terminalValue, total = terminalValue))
          
        } else {
          
          terminalValue <- terminalCalculator(type = "DIV", stage = stage, tabl = tabl, xVal = x, gr = grabValue("GR", tabl) )
          # (((grabValue("EPS") * (1 + x$t_growth_nInput) * (x$t_payout_nInput))/(x$t_coe_nInput - x$t_growth_nInput))/(1 + grabValue("EPS"))) + pvPerShare
          return(list(firstStage = grabValue("GR", tabl), secondStage = grabValue("COE", tabl), total = terminalValue))
        }
        
      } else {
        
        if("m_EqRR" %in% name){

          #Average return in terminal phase
          terminalValue <- terminalCalculator(type = "FCFE", stage = stage, tabl = tabl, xVal = x, gr = x$t_growth_nInput)
          # pvPerShare + (tValue / discountRate)
          return(list(firstStage = pvPerShare, secondStage = terminalValue, total = pvPerShare + terminalValue))
        
          } else if ("EBIT" %in% name) {
            
            terminalValue <- terminalCalculator(type = "FCFF", stage = stage, tabl = tabl, xVal = x, gr = x$t_growth_nInput)
            
            return(list(firstStage = pvPerShare, secondStage = terminalValue, total = pvPerShare + terminalValue))
          
            } else {
            
          pvDividends <- sum(grabValue("Dividends", tabl, which = seq_col(tabl))/(grabValue("#Shares", tabl))/grabValue("COE", tabl, which = seq_col(tabl)))
          terminalValue <- terminalCalculator(type = "DIV", stage = stage, tabl = tabl, xVal = x, gr = x$t_growth_nInput)
          # (((grabValue("EPS") * (1 + x$t_growth_nInput) * (x$t_payout_nInput))/(x$t_coe_nInput - x$t_growth_nInput))/(1 + grabValue("EPS"))) + pvPerShare
          return(list(firstStage = pvDividends, secondStage = terminalValue, total = pvDividends + terminalValue))
          
          }
        
      }
      
    })()
}

#--------------------------------------------------------------------------------------------------------------

#need to modify this function based on stage
terminalPlot <- function(firstStage, secondStage, stage, total){
  
  one <- stage == "One"
  
  tryCatch(if(one){
    cbind(PV = "PV", total) |> 
      qDF() |>  
      e_charts(PV) |> 
      e_bar(total) |> 
      e_tooltip()
  } else {
    cbind(PV = "PV", firstStage, secondStage, total) |> 
      qDF() |>  
      e_charts(PV) |> 
      e_bar(firstStage, stack = "grp") |> 
      e_bar(secondStage, stack = "grp") |> 
      e_bar(total) |> 
      e_tooltip()
  }, error = function(e) e, finally = "")
}

#------------------------------------------------------------------------------------------------------------

hypoGraph <- function(tabl, stage, ...){
  
  #sequence the Growth Rate, needed for hypothetical growth rate
  seqRate <- function(gRate){
    
    return(if(!is.null(gRate)) seq(0, gRate + 0.1, 0.01) else 0)
  }
  
    df <- list(...) |> 
    (\(x){
      
      gRates <- switch(stage,
                       One = seqRate(grabValue("GR", tabl)),
                       seqRate(x$t_growth_nInput)
      )
      
      pvPerShare <- sum(grabValue("Present Value",tabl, which = seq_col(tabl)))/grabValue("#Shares", tabl)
      
      #if FCF
      value <- if(length(x$dividends_nInput)){
        terminalValue <- terminalCalculator(type = "DIV", stage = stage, tabl = tabl, xVal = x, gr = gRates)
        
        switch (stage,
                One = terminalValue,
                {
                  pvDividends <- sum(grabValue("Dividends", tabl, which = seq_col(tabl))/(grabValue("#Shares", tabl))/grabValue("COE", tabl, which = seq_col(tabl)))
                  pvDividends + terminalValue
                })

      } else if(length(x$reinv_nInput)) {
        
        terminalValue <- terminalCalculator(type = "FCFE", stage = stage, tabl = tabl, xVal = x, gr = gRates)
        
        
        switch (stage,
                One = terminalValue,
                {
                  pvPerShare + terminalValue
                })
      } else {
        terminalValue <- terminalCalculator(type = "FCFF", stage = stage, tabl = tabl, xVal = x, gr = gRates)
        
        switch (stage,
                One = terminalValue,
                {
                  
                  pvPerShare + terminalValue
                })
        }
      
      df <- if(length(value)) data.frame(growth_rate = gRates, value = value)[is.finite(value), ] else data.frame(growth_rate = gRates, value = 0)

      return(df)
    
      })()
    
    df |> 
    (\(x)
     
     {x |> 
         e_charts(growth_rate) |> 
         e_line(value, name = "Present Value", legend = T) |> 
         e_tooltip() |> 
         e_axis_labels( y = "Present Value", x = 'Growth Rate') |> 
         e_x_axis(formatter = e_axis_formatter(style = "percent"))
    })()
  
}


# -------------------------------------------------------------------------------------------------------------
#variable names and values that are used by multiple functions, mostly for numeric inputs

getVars <- function(name){
  
  #capm doesn't differ between the types but we need to give them an environment reference
  CAPM = rep(list(c("Rf" = "rf", "Beta" = "beta", "ErP" = "erp")), 3) |> 
    setNames(list("DDM", "FCFE", "FCFF"))
  
  capm_unique <- CAPM[[1]]
  
  sharedMain = c("#Shares" = "numShares","GR" = "gr", "Time Span" = "tspan")
  
  sharedCashFlowMain <- c("CapEx" = "capEx", "Change Wk Cap" = "cwc")
  
  
  mainL = list(DDM = c("Dividends" = "dividends", "Net Income" = "net_inc", sharedMain),
               FCFE = c("Net Income" = "net_inc", sharedCashFlowMain, "m_EqRR" = "reinv", sharedMain),
               FCFF = c("Market Cap" = "market_cap", "EBIT" = "ebit", "Tax Rate" = "taxRate", "Debt" = "debt", "COD" = "cod", "Cash" = "cash", "m_ReinvRate" = "reinvR", sharedCashFlowMain,  sharedMain))
  
  extraL = list(DDM = c(),
                FCFE = c( "Depreciation" = "depreciation", "Debt Iss" = "debt_iss", 
                         "Debt Paid" = "debt_paid", "Book Val" =  "book_val"),
                FCFF = c("Depreciation" = "depreciation", "Debt Iss" = "debt_iss", 
                         "Debt Paid" = "debt_paid", "Book Val" =  "book_val"))
  
  sharedTerminal <- c("T_growth" = "t_growth")
  
  #values that are calculated that will show up in table but not in the numeric input
  terminalL = list(DDM = c("T_Payout Ratio" = "t_payout", sharedTerminal,  "T_COE"="t_coe"), 
                   FCFE = c("T_eqRR" = "t_eq_rr", sharedTerminal, "T_COE"="t_coe"), 
                   FCFF = c("T_RR" = "t_rr", sharedTerminal,  "T_COC"="t_coc"))
  
  sharedIntermediate <- list("D_growth" = "d_growth", "D_Time Span" = "d_tspan")
  
  intermediateL <- list(DDM = c("D_Payout Ratio" = "d_payout",  "D_COE" = "d_coe", sharedIntermediate), 
                        FCFE = c("D_eqRR" = "d_eq_rr",  "D_COE" = "d_coe", sharedIntermediate),
                        FCFF = c("D_RR" = "d_ReinvR",  "D_COC" = "d_coc", sharedIntermediate))
  
  get(name, envir = sys.frame(sys.nframe()))
}