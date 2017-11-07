function ndoys, year
  doy, year, 1,1, day_of_year=doyr1
  doy, year, 12,31, day_of_year=doyr2
  return,fix(doyr2-doyr1+1)
end

pro ndoys
  print,ndoys(2008)
  print,ndoys(2009)
end