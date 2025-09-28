import yahooquery as yq
import numpy as np


def get_financials(ticker):

    stock = yq.Ticker(ticker)
    all_data = stock.all_financial_data()
    
    if(type(all_data) != str):
        required_metrics = ["CashDividendsPaid", "NetIncome", "DilutedAverageShares", "CapitalExpenditure", "ChangeInWorkingCapital", "EBIT", "TaxRateForCalcs", "CashAndCashEquivalents", "MarketCap", "TotalDebt", "DepreciationAndAmortization", "RepaymentOfDebt", "TangibleBookValue"]
        columns = all_data.columns
    
        #metrics found that can be used
        found_mets  = columns[np.isin(columns, required_metrics)]
        set_diff = set(required_metrics).difference(found_mets)
        
        #return the first row, which will be in matrix format in R
        mat = ((all_data.loc[:, found_mets].iloc[all_data.shape[0] - 1, :]))
        
        mat[mat.index.isin(["CapitalExpenditure", "CashDividendsPaid", "RepaymentOfDebt","ChangeInWorkingCapital"])] = mat[mat.index.isin(["CapitalExpenditure", "CashDividendsPaid", "RepaymentOfDebt","ChangeInWorkingCapital"])].abs()
        
        #get series on per share basis, to do that we don't tuch taxRate
        not_tax_mets = mat[mat.index != "TaxRateCalcs"]
        #non tax rate metrics on per share basis
        per_share_vals = not_tax_mets.div(mat.loc["DilutedAverageShares"])
        
        mat[mat.index != "TaxRateCalcs"] = per_share_vals
        
        if(len(set_diff) > 0):
            message = f"{set_diff.pop() if len(set_diff) > 0 else None} was not found in search, it should be entered manually"
            return mat, message
        else:
            return mat
    else:
        return None


