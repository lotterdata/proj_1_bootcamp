possible <- outer(agencies$id,congress$DistrictCode, function(x,y) str_c(x,y)) %>%
        array()


existing <- outer(unique(contracts$Agency.Code),unique(contracts$pop_cd), function(x,y) str_c(x,y)) %>%
  array()
