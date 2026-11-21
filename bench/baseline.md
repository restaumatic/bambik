# bambik on Diner — 2026-11-21, 3ab462cb+dirty

11th Gen Intel(R) Core(TM) i7-11850H @ 2.50GHz, 16 threads, 63 GB; Chrome/158.0.8127.36; node v25.6.0; 1042 s.

### View model in node: one feed's reads per station (ms)

| station    | 1×    | 4×     | 16×     | 64×     | growth |
| ---------- | ----- | ------ | ------- | ------- | ------ |
| Magazyn    | 0.459 | 1.895  | 7.906   | 35.172  | 1.04   |
| Sala       | 0.447 | 1.987  | 8.955   | 37.376  | 1.07   |
| Kuchnia    | 0.071 | 0.34   | 1.759   | 9.639   | 1.18   |
| Wydawka    | 0.032 | 0.116  | 0.57    | 2.453   | 1.05   |
| Raport     | 0.157 | 0.684  | 3.38    | 16.567  | 1.12   |
| Statystyki | 5.523 | 28.181 | 153.693 | 805.723 | 1.2    |

The costliest reads at the largest scale:

| read                         | 1×    | 4×    | 16×    | 64×     | growth |
| ---------------------------- | ----- | ----- | ------ | ------- | ------ |
| Statystyki: isLastPage       | 0.783 | 4.218 | 21.436 | 120.622 | 1.21   |
| Statystyki: soldAmountLine   | 0.774 | 3.965 | 21.499 | 116.824 | 1.21   |
| Statystyki: searchState      | 0.798 | 3.863 | 20.575 | 114.561 | 1.2    |
| Statystyki: isFirstPage      | 0.819 | 4.189 | 24.318 | 114.347 | 1.2    |
| Statystyki: soldQuantityLine | 0.779 | 3.992 | 21.961 | 113.449 | 1.2    |
| Statystyki: salesPage        | 0.778 | 4.053 | 21.735 | 112.562 | 1.2    |
| Statystyki: pageLine         | 0.773 | 3.833 | 21.871 | 112.104 | 1.2    |
| Magazyn: stockPage           | 0.213 | 0.863 | 4.063  | 19.058  | 1.08   |
| Sala: menuPage               | 0.169 | 0.851 | 4.443  | 18.512  | 1.14   |
| Raport: paidPage             | 0.067 | 0.318 | 1.82   | 9.842   | 1.21   |

Generating the day (`dayAt`): 1× 1.29 ms, 4× 6.39 ms, 16× 39.2 ms, 64× 73.31 ms.

### Responsiveness: what one doing costs the browser (ms)

Script, style and layout together; the split is the median at full speed. The budget is on the 95th percentile at a quarter of the speed: 100 ms for something done, 16 ms for a second passing, 1 s for the start.

| done                                     | n  | p50   | p95   | script / style / layout | p50 at ¼ | p95 at ¼ | budget   |
| ---------------------------------------- | -- | ----- | ----- | ----------------------- | -------- | -------- | -------- |
| stock: a portion more                    | 30 | 3.57  | 5.84  | 2.96 / 0.4 / 0.3        | 14.38    | 21.25    | within   |
| stock: a portion fewer                   | 30 | 3.51  | 5.12  | 2.85 / 0.41 / 0.3       | 14.62    | 18.43    | within   |
| stock: a key in a portion count          | 30 | 2.56  | 3.03  | 2.32 / 0 / 0.19         | 10.3     | 12.52    | within   |
| stock: a key in a price                  | 30 | 2.38  | 2.96  | 2.15 / 0 / 0.17         | 9.36     | 10.71    | within   |
| stock: a page turned                     | 30 | 19.62 | 29.27 | 12.66 / 4.7 / 2.6       | 86.76    | 105.63   | over 100 |
| stock: a group picked                    | 30 | 21.02 | 29.58 | 11.92 / 6.51 / 2.73     | 91.18    | 121.25   | over 100 |
| stock: a key in the search               | 32 | 4.44  | 22.21 | 3.78 / 0.27 / 0.29      | 16.89    | 71.47    | within   |
| hall: a dish added                       | 30 | 3.84  | 7.82  | 3.36 / 0.2 / 0.34       | 15.78    | 27.02    | within   |
| hall: a portion more on a line           | 30 | 5.02  | 7.47  | 4.01 / 0.58 / 0.41      | 19.67    | 24.76    | within   |
| hall: a portion fewer on a line          | 30 | 5.42  | 7.24  | 4.25 / 0.75 / 0.49      | 22.5     | 27.48    | within   |
| hall: a table picked                     | 30 | 6.11  | 7.42  | 4.19 / 1.12 / 0.86      | 25.24    | 34.35    | within   |
| hall: a page turned                      | 30 | 7.85  | 10.75 | 6.02 / 1.07 / 0.84      | 31.11    | 37.28    | within   |
| hall: a group picked                     | 30 | 8.64  | 13.23 | 6.05 / 1.91 / 0.74      | 36.33    | 48.13    | within   |
| hall: a key in the search                | 32 | 5.75  | 7.74  | 4.45 / 0.38 / 0.7       | 23.32    | 31.19    | within   |
| kitchen: a page turned                   | 30 | 13.96 | 50.14 | 8.94 / 3.02 / 1.78      | 55.28    | 68.5     | within   |
| kitchen: a line plated                   | 7  | 4.05  | 4.75  | 2.93 / 0.71 / 0.38      | 15.3     | 15.72    | within   |
| kitchen: an order passed on, asked       | 12 | 4.3   | 6.44  | 0.38 / 3.64 / 0.4       | 19.55    | 27.43    | within   |
| kitchen: an order passed on, confirmed   | 12 | 18.03 | 36.23 | 3.85 / 11.32 / 2.87     | 62.87    | 73.39    | within   |
| pass: a payment, asked                   | 10 | 2.56  | 4.93  | 0.49 / 1.76 / 0.41      | 10.09    | 19.14    | within   |
| pass: a payment, confirmed               | 10 | 11.75 | 14.75 | 1.83 / 7.85 / 1.76      | 29.99    | 42       | within   |
| report: a page turned                    | 30 | 3.45  | 4.87  | 2.24 / 0.76 / 0.62      | 12.99    | 18.11    | within   |
| statistics: a page turned                | 30 | 11.65 | 15.97 | 10.45 / 0.76 / 0.57     | 43.52    | 55.69    | within   |
| statistics: a group picked               | 30 | 14.67 | 17.77 | 12.25 / 1.8 / 0.6       | 59.56    | 71.23    | within   |
| statistics: a key in the search          | 32 | 13.03 | 23.36 | 12.82 / 0.15 / 0.53     | 55.03    | 68.4     | within   |
| app: a second passes at Magazyn          | 6  | 3.3   | 5.15  | 3.35 / 0.16 / 0         | 10.67    | 17.96    | over 16  |
| app: a second passes at Sala             | 6  | 3.01  | 3.39  | 2.95 / 0.17 / 0         | 12.2     | 14.51    | within   |
| app: a second passes at Kuchnia          | 6  | 1.47  | 1.74  | 1.29 / 0.24 / 0         | 5.87     | 6.22     | within   |
| app: a second passes at Wydawka          | 6  | 1.2   | 1.71  | 1.06 / 0.23 / 0         | 3.37     | 5.9      | within   |
| app: a second passes at Raport           | 6  | 1.38  | 1.93  | 1.4 / 0.15 / 0          | 3.86     | 6.64     | within   |
| app: a second passes at Statystyki       | 6  | 7.94  | 10.5  | 7.82 / 0.23 / 0         | 32.23    | 43.22    | over 16  |
| app: switched to Magazyn                 | 5  | 16.63 | 19.58 | 5.21 / 8.61 / 2.82      | 71.49    | 137.45   | over 100 |
| app: switched to Sala                    | 5  | 14.97 | 18.94 | 7.1 / 5.56 / 1.86       | 73.44    | 121.41   | over 100 |
| app: switched to Kuchnia                 | 5  | 10.45 | 12.97 | 3.14 / 5.63 / 1.73      | 44.51    | 52.64    | within   |
| app: switched to Wydawka                 | 5  | 5.85  | 6.38  | 2.05 / 2.75 / 1.01      | 26.25    | 28.34    | within   |
| app: switched to Raport                  | 5  | 5.01  | 6.31  | 2.38 / 2.07 / 0.64      | 20.97    | 25.5     | within   |
| app: switched to Statystyki              | 5  | 23.62 | 30.61 | 18.27 / 4.42 / 1.21     | 82.4     | 105.74   | over 100 |
| app: started, until it waits for its day | 6  | 47.42 | 49.26 | 41.97 / 2.15 / 3.39     | 181.24   | 195.77   | within   |
| app: started, the day drawn              | 6  | 53.16 | 58.22 | 38.8 / 4.02 / 9.87      | 214.51   | 226.63   | within   |

Where the time of a station's doings goes (CPU profile, self time by layer):

| station    | ms profiled | view model | carrier | design system | browser | gc |
| ---------- | ----------- | ---------- | ------- | ------------- | ------- | -- |
| Stock      | 1451        | 5%         | 20%     | 35%           | 36%     | 4% |
| Hall       | 893         | 15%        | 33%     | 8%            | 40%     | 4% |
| Kitchen    | 820         | 2%         | 16%     | 25%           | 54%     | 4% |
| Pass       | 311         | 1%         | 3%      | 17%           | 77%     | 1% |
| Report     | 116         | 8%         | 23%     | 7%            | 58%     | 4% |
| Statistics | 718         | 58%        | 10%     | 5%            | 22%     | 5% |

Stock, the functions that took most: (program) 509.57 ms; scrollToTab (node_modules/@material/web/tabs/internal/tabs.js) 76.13 ms; (garbage collector) 58.81 ms; insertBefore 34.33 ms; setAttribute 34.08 ms.
Hall, the functions that took most: (program) 346.58 ms; (garbage collector) 37.44 ms; (anonymous) (Effect) 31.3 ms; __do3 (PUI) 26.24 ms; setAttribute 22.24 ms.
Kitchen, the functions that took most: (program) 425.82 ms; animate 47.28 ms; showModal 31.84 ms; (garbage collector) 30.86 ms; (anonymous) (Effect) 15.11 ms.
Pass, the functions that took most: (program) 233.12 ms; animate 17.38 ms; showModal 15.26 ms; (garbage collector) 2.04 ms; (anonymous) 1.96 ms.
Report, the functions that took most: (program) 65.62 ms; setAttribute 5.19 ms; (garbage collector) 4.96 ms; (anonymous) (Effect) 3.38 ms; $$default (Data.Variant) 2.25 ms.
Statistics, the functions that took most: (program) 152.92 ms; go (Data.Map.Internal) 106.78 ms; mergeFromTo (Data.Array) 70.92 ms; (anonymous) (Data.Map.Internal) 44.43 ms; (anonymous) (Data.Ord) 39.71 ms.

No exception and no console warning.

### Large data: a station alone on days of growing load (ms)

The standard day is 1×; 64× is 19456 dishes, 2688 tables and 16128 orders. Growth is the exponent k of cost ≈ c·scale^k: 0 is flat, 1 linear, above 1 a finding.

| station    | measured        | 1×    | 4×     | 16×    | 64×     | growth |
| ---------- | --------------- | ----- | ------ | ------ | ------- | ------ |
| Stock      | drawn           | 84.66 | 99.93  | 171.16 | 284.59  | 0.3    |
| Stock      | a second passes | 2.77  | 7.4    | 17.14  | 59.3    | 0.72   |
| Stock      | a portion more  | 5.65  | 9.19   | 23.54  | 90.86   | 0.67   |
| Stock      | DOM nodes       | 6184  | 6192   | 6192   | 6192    | 0      |
| Stock      | heap (MB)       | 6.3   | 5.9    | 10.2   | 17.7    | 0.26   |
| Hall       | drawn           | 86.05 | 144.75 | 289.1  | 839.64  | 0.54   |
| Hall       | a second passes | 3.75  | 7.59   | 27.29  | 91.39   | 0.78   |
| Hall       | a dish added    | 4.33  | 10.03  | 27     | 106.46  | 0.76   |
| Hall       | DOM nodes       | 2457  | 5121   | 15777  | 54029   | 0.75   |
| Hall       | heap (MB)       | 7.2   | 12     | 18.7   | 51.6    | 0.46   |
| Kitchen    | drawn           | 69.02 | 76.86  | 102.51 | 168     | 0.21   |
| Kitchen    | a second passes | 1.82  | 3.12   | 3.85   | 9.65    | 0.38   |
| Kitchen    | a page turned   | 15.14 | 15.63  | 17.41  | 25.4    | 0.12   |
| Kitchen    | DOM nodes       | 3286  | 3143   | 3325   | 2896    | -0.02  |
| Kitchen    | heap (MB)       | 4.7   | 7.4    | 9.6    | 13.4    | 0.25   |
| Pass       | drawn           | 48.95 | 56.82  | 87.81  | 142.49  | 0.26   |
| Pass       | a second passes | 0.85  | 1.31   | 2.25   | 5.81    | 0.45   |
| Pass       | a payment taken | 15.32 | 17.01  | 18.96  | 24.55   | 0.11   |
| Pass       | DOM nodes       | 922   | 924    | 924    | 924     | 0      |
| Pass       | heap (MB)       | 4.5   | 4.6    | 7.6    | 15.5    | 0.3    |
| Report     | drawn           | 53.3  | 57.54  | 87.92  | 171.15  | 0.28   |
| Report     | a second passes | 1.28  | 1.96   | 4.08   | 15.18   | 0.59   |
| Report     | a page turned   | 4.1   | 4.22   | 6.78   | 23.84   | 0.42   |
| Report     | DOM nodes       | 301   | 301    | 301    | 301     | 0      |
| Report     | heap (MB)       | 5     | 6.4    | 8.9    | 36.2    | 0.45   |
| Statistics | drawn           | 89.88 | 153.32 | 420.79 | 1695.54 | 0.71   |
| Statistics | a second passes | 9.51  | 29.9   | 140.56 | 1088.78 | 1.14   |
| Statistics | a page turned   | 12.38 | 38.19  | 161.68 | 840.92  | 1.02   |
| Statistics | DOM nodes       | 924   | 924    | 924    | 905     | 0      |
| Statistics | heap (MB)       | 4.9   | 8.3    | 18     | 44.3    | 0.53   |

Where drawing the day at 64× goes (CPU profile, self time by layer):

| station    | ms profiled | view model | carrier | design system | browser | gc  |
| ---------- | ----------- | ---------- | ------- | ------------- | ------- | --- |
| Stock      | 377         | 48%        | 22%     | 16%           | 7%      | 6%  |
| Hall       | 1088        | 18%        | 42%     | 13%           | 19%     | 8%  |
| Kitchen    | 226         | 54%        | 11%     | 11%           | 15%     | 10% |
| Pass       | 188         | 57%        | 9%      | 11%           | 14%     | 9%  |
| Report     | 222         | 65%        | 8%      | 7%            | 11%     | 9%  |
| Statistics | 1851        | 89%        | 1%      | 2%            | 2%      | 7%  |

With pagination off, everything listed is on screen. A row's cost is the difference to the paged station over the difference in rows:

| station    | scale | listed | drawn   | per row | a second passes | per row | DOM nodes |
| ---------- | ----- | ------ | ------- | ------- | --------------- | ------- | --------- |
| Stock      | 1×    | 304    | 678.1   | 2.019   | 19.92           | 0.058   | 144136    |
| Stock      | 4×    | 1216   | 2312.53 | 1.835   | 102.02          | 0.078   | 641133    |
| Hall       | 1×    | 346    | 164.09  | 0.279   | 12.05           | 0.03    | 6655      |
| Hall       | 4×    | 1384   | 446.76  | 0.253   | 50.79           | 0.036   | 22999     |
| Kitchen    | 1×    | 20     | 80.87   | 1.185   | 2.61            | 0.079   | 6085      |
| Kitchen    | 4×    | 80     | 183.34  | 1.521   | 7.03            | 0.056   | 22345     |
| Pass       | 1×    | 10     | 53.17   | —       | 0.82            | —       | 922       |
| Pass       | 4×    | 40     | 69.81   | 0.433   | 2.57            | 0.042   | 2422      |
| Report     | 1×    | 220    | 101.99  | 0.233   | 7.02            | 0.027   | 2598      |
| Report     | 4×    | 877    | 254.45  | 0.227   | 22.42           | 0.024   | 9825      |
| Statistics | 1×    | 263    | 153.79  | 0.254   | 20.22           | 0.042   | 3442      |
| Statistics | 4×    | 1040   | 370.24  | 0.211   | 81.07           | 0.05    | 11212     |

### Correctness under stress: a random walk against the view model

3000 steps of real input (seed 20261120, 167 s): after every one the station on screen read exactly what the view model says of the reference model. The day ended with 310 orders, 288 of them paid.

Done along the way: open ×1177, next page ×326, previous page ×239, add ×186, search ×162, order ×159, serve table ×136, plate ×125, stock + ×76, stock − ×64, price of ×61, group ×60, portions of ×53, line + ×48, zero ×45, send ×41, remove ×27, line − ×15.

No exception and no console warning.

### Races: the application driven faster than a person can

- holds: twelve clicks on a dish with no wait between them put twelve portions on the order
- holds: three digits typed while 3 seconds pass stay as typed, the caret after them
- holds: and are the stock when the stockroom is come back to
- holds: a dish clicked while the order was on its way went with it or stayed in stock, never both or neither
- holds: the last page losing its only row falls back to the page before
- holds: a payment confirmed twice at once is taken once

No exception and no console warning.

### Reliability: left running

9433 random clicks over 240 s under the clock, sampled every 25 after a forced collection. The slope is over the second half of the run:

|           | first sample | last sample | slope per minute |
| --------- | ------------ | ----------- | ---------------- |
| heap (MB) | 6.3          | 10          | 0.26             |
| DOM nodes | 7955         | 14837       | 296.44           |
| listeners | 872          | 2327        | 42.23            |

What one doing leaves behind for good (twenty of them between two forced collections, a station alone):

| done                      | DOM nodes | listeners | heap (KB) |
| ------------------------- | --------- | --------- | --------- |
| stockroom: a page turned  | -0.1      | 0         | 18.5      |
| hall: a page turned       | -0.1      | 0         | 15.2      |
| hall: a group picked      | 0         | 0         | 13.7      |
| hall: a table picked      | 0         | 0         | 4.8       |
| kitchen: a page turned    | -0.1      | 0         | 15.2      |
| report: a page turned     | -0.1      | 0         | 8         |
| statistics: a page turned | -0.1      | 0         | 8         |

900 station switches after a warm-up of twelve: heap 7.2 → 7.8 MB, DOM nodes 11623 → 11623, listeners 1275 → 1275.

The clock, watched for 60 s with the hall open: 1 of its seconds pass in a second. Across 7.2 s with the page frozen for 6 of them it counted 1.

No exception and no console warning.

### Application size: synthetic applications of growing width

Each station is a display, an editor, a keyed table of five rows and three buttons with their folds; one model, one knot. Compile time is for the two modules (view and view model), over the 0.3 s the compiler needs with nothing to do. Growth is the exponent against the number of stations.

|                                   | 6 stations | 12 stations | 24 stations | 48 stations | growth |
| --------------------------------- | ---------- | ----------- | ----------- | ----------- | ------ |
| compile (s)                       | 0.3        | 0.7         | 2.2         | 8.9         | 1.63   |
| bundle, minified (KB)             | 499.6      | 520.8       | 593.5       | 857.9       | 0.25   |
| bundle, gzipped (KB)              | 95.6       | 97.2        | 100.6       | 108.5       | 0.06   |
| drawn (ms)                        | 45.38      | 55.7        | 59.68       | 79.91       | 0.25   |
| DOM nodes                         | 966        | 1356        | 2136        | 3696        | 0.65   |
| a tick fed, one station open (ms) | 0.63       | 0.62        | 0.84        | 0.94        | 0.22   |
| a button clicked (ms)             | 1.33       | 1.25        | 2.02        | 1.56        | 0.14   |

No exception and no console warning.

