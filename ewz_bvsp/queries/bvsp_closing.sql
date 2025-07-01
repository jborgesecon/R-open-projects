SELECT 
    ticker,
    ref_date,
    price_close
FROM stock_market.ewz_bvsp
WHERE ticker LIKE('BVSP');