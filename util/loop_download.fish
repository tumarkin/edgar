#!/usr/local/bin/fish
for year in (seq 1993 2016)
  for qtr in (seq 1 4)
    echo "edgar $year $qtr"
    edgar $year $qtr
  end
end
