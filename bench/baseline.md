# bambik on Diner — 2026-11-22, 199b9603+dirty

11th Gen Intel(R) Core(TM) i7-11850H @ 2.50GHz, 16 threads, 63 GB; Chrome/158.0.8127.36; node v25.6.0; 1390 s.

### View model in node: one feed's reads per station (ms)

| station    | 1×    | 4×     | 16×     | 64×     | growth |
| ---------- | ----- | ------ | ------- | ------- | ------ |
| Magazyn    | 0.449 | 1.708  | 7.867   | 36.296  | 1.06   |
| Sala       | 0.443 | 1.958  | 8.877   | 36.33   | 1.06   |
| Kuchnia    | 0.064 | 0.3    | 1.696   | 8.704   | 1.19   |
| Wydawka    | 0.031 | 0.12   | 0.544   | 2.647   | 1.07   |
| Raport     | 0.141 | 0.595  | 2.977   | 14.908  | 1.13   |
| Statystyki | 5.215 | 27.042 | 142.959 | 793.881 | 1.21   |

The costliest reads at the largest scale:

| read                         | 1×    | 4×    | 16×    | 64×     | growth |
| ---------------------------- | ----- | ----- | ------ | ------- | ------ |
| Statystyki: salesPage        | 0.756 | 3.88  | 20.123 | 122.636 | 1.22   |
| Statystyki: searchState      | 0.752 | 3.825 | 19.86  | 113.985 | 1.21   |
| Statystyki: soldQuantityLine | 0.689 | 3.879 | 20.288 | 112.19  | 1.22   |
| Statystyki: pageLine         | 0.762 | 3.723 | 19.506 | 111.58  | 1.2    |
| Statystyki: soldAmountLine   | 0.732 | 3.927 | 19.551 | 110.963 | 1.2    |
| Statystyki: isLastPage       | 0.734 | 3.767 | 23.488 | 110.959 | 1.22   |
| Statystyki: isFirstPage      | 0.777 | 3.982 | 19.889 | 110.509 | 1.19   |
| Magazyn: stockPage           | 0.216 | 0.812 | 4.368  | 20.77   | 1.11   |
| Sala: menuPage               | 0.166 | 0.865 | 4.597  | 18.228  | 1.14   |
| Raport: paidPage             | 0.06  | 0.288 | 1.588  | 9.053   | 1.21   |

Generating the day (`dayAt`): 1× 1.78 ms, 4× 5.8 ms, 16× 33.98 ms, 64× 82.32 ms.

### Responsiveness: what one doing costs the browser (ms)

Everything the main thread does from the doing to the frame that shows it; the split is the median at full speed, the rest being painting, compositing and the browser's own — a frame with nothing done is its floor. The budget is on the 95th percentile at a quarter of the speed: 100 ms for something done, 16 ms for a second passing, 1 s for the start.

| done                                     | n  | p50   | p95   | script / style / layout / rest | p50 at ¼ | p95 at ¼ | budget   |
| ---------------------------------------- | -- | ----- | ----- | ------------------------------ | -------- | -------- | -------- |
| stock: a portion more                    | 30 | 8.83  | 12.45 | 2.69 / 0.54 / 0.32 / 5.3       | 34.28    | 49.59    | within   |
| stock: a portion fewer                   | 30 | 8.83  | 10.68 | 2.79 / 0.57 / 0.3 / 5.34       | 34.37    | 49.25    | within   |
| stock: a key in a portion count          | 30 | 5.8   | 6.68  | 2.19 / 0.17 / 0.23 / 2.99      | 21.42    | 25.5     | within   |
| stock: a key in a price                  | 30 | 5.72  | 6.87  | 2.21 / 0 / 0.23 / 3.09         | 20.56    | 24.37    | within   |
| stock: a page turned                     | 30 | 33.9  | 45.26 | 13 / 4.96 / 2.56 / 13.38       | 132.14   | 167.34   | over 100 |
| stock: a group picked                    | 30 | 36.84 | 49.84 | 12.05 / 6.46 / 2.68 / 15.67    | 149.61   | 199.48   | over 100 |
| stock: a key in the search               | 32 | 8.53  | 30.05 | 3.89 / 0.26 / 0.33 / 4.07      | 28.63    | 125.07   | over 100 |
| hall: a dish added                       | 30 | 7.44  | 13.38 | 3.65 / 0.3 / 0.37 / 3.27       | 26.71    | 42.51    | within   |
| hall: a portion more on a line           | 30 | 9.81  | 13.85 | 3.95 / 0.79 / 0.47 / 4.73      | 42.47    | 60.11    | within   |
| hall: a portion fewer on a line          | 30 | 10.29 | 12.16 | 4.07 / 0.96 / 0.5 / 4.64       | 47.06    | 65.67    | within   |
| hall: a table picked                     | 30 | 9.54  | 13.17 | 4.33 / 1.3 / 0.86 / 3.58       | 41.03    | 45.81    | within   |
| hall: a page turned                      | 30 | 12.32 | 14.91 | 5.91 / 1.27 / 0.8 / 4.38       | 47.64    | 70.01    | within   |
| hall: a group picked                     | 30 | 14.38 | 22.43 | 6.14 / 2.28 / 0.78 / 4.83      | 54.63    | 68.15    | within   |
| hall: a key in the search                | 32 | 8.47  | 12.12 | 4.55 / 0.43 / 0.72 / 2.78      | 36.11    | 52.42    | within   |
| kitchen: a page turned                   | 30 | 22.5  | 27.99 | 10.32 / 3.12 / 1.73 / 7.76     | 81.82    | 120.31   | over 100 |
| kitchen: a line plated                   | 7  | 7.5   | 10.5  | 2.57 / 0.9 / 0.39 / 3.68       | 27.17    | 37.75    | within   |
| kitchen: an order passed on, asked       | 12 | 9.43  | 11.88 | 0.46 / 3.86 / 0.37 / 4.5       | 38.73    | 52.63    | within   |
| kitchen: an order passed on, confirmed   | 12 | 39.7  | 48.77 | 4.19 / 11.18 / 2.93 / 21.51    | 120.23   | 165.14   | over 100 |
| pass: a payment, asked                   | 10 | 6.35  | 9.58  | 0.59 / 2.05 / 0.41 / 3.47      | 24.09    | 38.75    | within   |
| pass: a payment, confirmed               | 10 | 26.25 | 32.39 | 1.72 / 7.78 / 1.66 / 15.29     | 58.43    | 66.87    | within   |
| report: a page turned                    | 30 | 6.77  | 8.9   | 2.42 / 0.94 / 0.63 / 2.77      | 23.47    | 33.36    | within   |
| statistics: a page turned                | 30 | 14.56 | 17.91 | 10.08 / 0.9 / 0.6 / 3.08       | 53.45    | 69.19    | within   |
| statistics: a group picked               | 30 | 21.52 | 29.41 | 14.71 / 2.18 / 0.61 / 4.27     | 72.19    | 83.32    | within   |
| statistics: a key in the search          | 32 | 15.93 | 23.94 | 13.01 / 0.17 / 0.55 / 2.22     | 59.04    | 82.04    | within   |
| app: a frame with nothing done           | 6  | 0.95  | 1.08  | 0.09 / 0.15 / 0 / 0.75         | 2.63     | 9.88     | within   |
| app: a second passes at Magazyn          | 6  | 2.37  | 5.62  | 0.24 / 0.11 / 0 / 2.36         | 5.45     | 9.23     | within   |
| app: a second passes at Sala             | 6  | 3.92  | 8.44  | 0.24 / 0.22 / 0 / 4.01         | 3.8      | 4.85     | within   |
| app: a second passes at Kuchnia          | 6  | 1.92  | 2.43  | 0.29 / 0.11 / 0 / 1.69         | 4.37     | 5.49     | within   |
| app: a second passes at Wydawka          | 6  | 1.64  | 2.06  | 0.43 / 0.1 / 0 / 1.23          | 4.22     | 6.07     | within   |
| app: a second passes at Raport           | 6  | 1.49  | 2.44  | 0.4 / 0.08 / 0 / 1.07          | 5.01     | 8.75     | within   |
| app: a second passes at Statystyki       | 6  | 1.49  | 2.23  | 0.22 / 0.08 / 0 / 1.24         | 3.75     | 7.38     | within   |
| app: switched to Magazyn                 | 20 | 25.88 | 31.6  | 4.38 / 8.03 / 2.74 / 10.8      | 123.93   | 135.66   | over 100 |
| app: switched to Sala                    | 20 | 25.45 | 27.75 | 7.26 / 5.74 / 1.7 / 10.08      | 114.62   | 122.6    | over 100 |
| app: switched to Kuchnia                 | 20 | 16.31 | 18.07 | 2.9 / 5.36 / 1.61 / 6.27       | 78.71    | 85.14    | within   |
| app: switched to Wydawka                 | 20 | 11.92 | 13.72 | 1.9 / 3.34 / 0.99 / 5.96       | 47.95    | 56.62    | within   |
| app: switched to Raport                  | 20 | 9.34  | 10.79 | 1.85 / 2.67 / 0.63 / 4.18      | 33.66    | 39.74    | within   |
| app: switched to Statystyki              | 20 | 26.08 | 38.15 | 15.27 / 4.63 / 1.05 / 5.54     | 125.5    | 133.76   | over 100 |
| app: started, until it waits for its day | 6  | 57.98 | 68.74 | 39.58 / 2.4 / 3.61 / 13.13     | 229.59   | 237.33   | within   |
| app: started, the day drawn              | 6  | 69.92 | 75.85 | 40.94 / 4.08 / 11.38 / 14.68   | 241.53   | 250.73   | within   |

Where the time of a station's doings goes (CPU profile, self time by layer):

| station    | ms profiled | view model | carrier | design system | browser | gc |
| ---------- | ----------- | ---------- | ------- | ------------- | ------- | -- |
| Stock      | 1518        | 5%         | 19%     | 31%           | 41%     | 4% |
| Hall       | 940         | 15%        | 30%     | 7%            | 43%     | 5% |
| Kitchen    | 780         | 2%         | 16%     | 22%           | 56%     | 4% |
| Pass       | 300         | 1%         | 4%      | 17%           | 77%     | 1% |
| Report     | 108         | 5%         | 24%     | 9%            | 56%     | 6% |
| Statistics | 732         | 55%        | 9%      | 6%            | 23%     | 6% |

Stock, the functions that took most: (program) 604.16 ms; scrollToTab (node_modules/@material/web/tabs/internal/tabs.js) 73.07 ms; (garbage collector) 58.85 ms; showStringImpl (Data.Show) 32.08 ms; setAttribute 30.08 ms.
Hall, the functions that took most: (program) 387.92 ms; (garbage collector) 49.05 ms; (anonymous) (Effect) 31.05 ms; setAttribute 25.81 ms; __do3 (PUI) 20.93 ms.
Kitchen, the functions that took most: (program) 425.73 ms; animate 45.43 ms; showModal 32.81 ms; (garbage collector) 29.07 ms; appendChild 11.73 ms.
Pass, the functions that took most: (program) 223.66 ms; animate 17.29 ms; showModal 16.82 ms; (garbage collector) 2.6 ms; (anonymous) 2.47 ms.
Report, the functions that took most: (program) 59.21 ms; (garbage collector) 6.1 ms; setAttribute 4.61 ms; (anonymous) (Effect) 3.86 ms; animate 1.84 ms.
Statistics, the functions that took most: (program) 158.88 ms; go (Data.Map.Internal) 102.8 ms; (anonymous) (Diner) 51.35 ms; (garbage collector) 46.39 ms; (anonymous) (Data.Ord) 45.95 ms.

No exception and no console warning.

### Large data: a station alone on days of growing load (ms)

The standard day is 1×; 64× is 19456 dishes, 2688 tables and 16128 orders. Growth is the exponent k of cost ≈ c·scale^k: 0 is flat, 1 linear, above 1 a finding. A cost is everything the main thread did up to the frame that shows the change, painting and compositing included; a second of a station with no clock is a frame with nothing done.

| station    | measured        | 1×     | 4×     | 16×    | 64×     | growth |
| ---------- | --------------- | ------ | ------ | ------ | ------- | ------ |
| Stock      | drawn           | 111.25 | 129.07 | 167.46 | 305.69  | 0.24   |
| Stock      | a second passes | 0.58   | 0.68   | 0.64   | 0.64    | 0.02   |
| Stock      | a portion more  | 9.32   | 14.86  | 28.54  | 92.8    | 0.54   |
| Stock      | DOM nodes       | 6184   | 6192   | 6192   | 6192    | 0      |
| Stock      | heap (MB)       | 6.2    | 5.9    | 10.2   | 20.5    | 0.3    |
| Hall       | drawn           | 113.39 | 157.5  | 311.75 | 910.58  | 0.5    |
| Hall       | a second passes | 1.42   | 1.41   | 2.17   | 1.84    | 0.09   |
| Hall       | a dish added    | 7.24   | 13.48  | 36.74  | 126.02  | 0.69   |
| Hall       | DOM nodes       | 2457   | 5121   | 15777  | 54023   | 0.75   |
| Hall       | heap (MB)       | 7.4    | 12     | 18.7   | 51.3    | 0.45   |
| Kitchen    | drawn           | 86.68  | 90.77  | 120.38 | 188.08  | 0.19   |
| Kitchen    | a second passes | 1.2    | 1.56   | 1.45   | 1.35    | 0.02   |
| Kitchen    | a page turned   | 22.1   | 23.31  | 24.06  | 31.69   | 0.08   |
| Kitchen    | DOM nodes       | 3286   | 3143   | 3325   | 2896    | -0.02  |
| Kitchen    | heap (MB)       | 4.9    | 7.3    | 9.3    | 15.8    | 0.27   |
| Pass       | drawn           | 62.62  | 73.88  | 91.88  | 165.27  | 0.23   |
| Pass       | a second passes | 1.45   | 1.44   | 0.97   | 1.13    | -0.08  |
| Pass       | a payment taken | 33     | 35.67  | 37.97  | 42.5    | 0.06   |
| Pass       | DOM nodes       | 922    | 924    | 924    | 924     | 0      |
| Pass       | heap (MB)       | 4.7    | 4.8    | 7.6    | 15.6    | 0.29   |
| Report     | drawn           | 63     | 67.17  | 96.47  | 168.53  | 0.24   |
| Report     | a second passes | 1.11   | 1.11   | 1.13   | 0.96    | -0.03  |
| Report     | a page turned   | 7.4    | 6.66   | 9.97   | 21.02   | 0.26   |
| Report     | DOM nodes       | 301    | 301    | 301    | 301     | 0      |
| Report     | heap (MB)       | 4.3    | 5.1    | 8      | 25.9    | 0.42   |
| Statistics | drawn           | 93.7   | 187.56 | 417.32 | 1634.66 | 0.68   |
| Statistics | a second passes | 0.68   | 0.72   | 0.62   | 0.7     | 0      |
| Statistics | a page turned   | 13.68  | 40.99  | 167.09 | 829.46  | 0.99   |
| Statistics | DOM nodes       | 924    | 924    | 924    | 905     | 0      |
| Statistics | heap (MB)       | 5.2    | 8.6    | 27.8   | 38      | 0.52   |

Where drawing the day at 64× goes (CPU profile, self time by layer):

| station    | ms profiled | view model | carrier | design system | browser | gc  |
| ---------- | ----------- | ---------- | ------- | ------------- | ------- | --- |
| Stock      | 396         | 48%        | 22%     | 17%           | 7%      | 7%  |
| Hall       | 1021        | 19%        | 41%     | 14%           | 18%     | 9%  |
| Kitchen    | 220         | 53%        | 10%     | 11%           | 18%     | 8%  |
| Pass       | 192         | 55%        | 9%      | 12%           | 16%     | 8%  |
| Report     | 206         | 59%        | 8%      | 7%            | 14%     | 12% |
| Statistics | 1759        | 89%        | 1%      | 2%            | 2%      | 7%  |

With pagination off, everything listed is on screen. A row's cost is the difference to the paged station over the difference in rows:

| station    | scale | listed | drawn   | per row | a second passes | per row | DOM nodes |
| ---------- | ----- | ------ | ------- | ------- | --------------- | ------- | --------- |
| Stock      | 1×    | 304    | 948.39  | 2.847   | 1.06            | 0.002   | 144136    |
| Stock      | 4×    | 1216   | 3288.46 | 2.62    | 1.31            | 0.001   | 641133    |
| Hall       | 1×    | 346    | 183.77  | 0.251   | 1.66            | 0.001   | 6655      |
| Hall       | 4×    | 1384   | 484.35  | 0.274   | 2.54            | 0.001   | 22999     |
| Kitchen    | 1×    | 20     | 109.43  | 2.275   | 1.75            | 0.055   | 6085      |
| Kitchen    | 4×    | 80     | 226.64  | 1.941   | 3.9             | 0.033   | 22345     |
| Pass       | 1×    | 10     | 63.52   | —       | 1.38            | —       | 922       |
| Pass       | 4×    | 40     | 94.29   | 0.68    | 2.14            | 0.023   | 2422      |
| Report     | 1×    | 220    | 116.52  | 0.256   | 2.11            | 0.005   | 2598      |
| Report     | 4×    | 877    | 265.79  | 0.229   | 4.35            | 0.004   | 9825      |
| Statistics | 1×    | 263    | 152.52  | 0.233   | 0.77            | 0       | 3442      |
| Statistics | 4×    | 1040   | 428.5   | 0.234   | 0.57            | 0       | 11212     |

### Correctness under stress: a random walk against the view model

3000 steps of real input (seed 20261120, 175 s): after every one the station on screen read exactly what the view model says of the reference model. The day ended with 310 orders, 288 of them paid.

Done along the way: open ×1177, next page ×326, previous page ×239, add ×186, search ×162, order ×159, serve table ×136, plate ×125, stock + ×76, stock − ×64, price of ×61, group ×60, portions of ×53, line + ×48, zero ×45, send ×41, remove ×27, line − ×15.

No exception and no console warning.

### Races: the application driven faster than a person can

- holds: twelve clicks on a dish with no wait between them put twelve portions on the order
- holds: three digits typed while 4 seconds pass stay as typed, the caret after them
- holds: and are the stock when the stockroom is come back to
- holds: a dish clicked while the order was on its way went with it or stayed in stock, never both or neither
- holds: the last page losing its only row falls back to the page before
- holds: a payment confirmed twice at once is taken once

No exception and no console warning.

### Reliability: left running

9650 random clicks over 240 s under the clock, sampled every 25 after a forced collection. The slope is over the second half of the run:

|           | first sample | last sample | slope per minute |
| --------- | ------------ | ----------- | ---------------- |
| heap (MB) | 6.3          | 9.9         | 0.27             |
| DOM nodes | 7954         | 14369       | 354.15           |
| listeners | 872          | 1569        | 48.33            |

What one doing leaves behind for good (twenty of them between two forced collections, a station alone):

| done                      | DOM nodes | listeners | heap (KB) |
| ------------------------- | --------- | --------- | --------- |
| stockroom: a page turned  | -0.1      | 0         | 18.5      |
| hall: a page turned       | -0.1      | 0         | 15.9      |
| hall: a group picked      | 0         | 0         | 14.3      |
| hall: a table picked      | 0         | 0         | 4.7       |
| kitchen: a page turned    | -0.1      | 0         | 53.6      |
| report: a page turned     | -0.1      | 0         | 8.7       |
| statistics: a page turned | -0.1      | 0         | 8.5       |

900 station switches after a warm-up of twelve: heap 7.3 → 7.9 MB, DOM nodes 11623 → 11623, listeners 1275 → 1275.

The clock, watched for 60 s with the hall open: 1 of its seconds pass in a second. Across 7.2 s with the page frozen for 6 of them it counted 1.

No exception and no console warning.

### Application size: synthetic applications of growing width

Each station is a display, an editor, a keyed table of five rows and three buttons with their folds; one model, one knot. Compile time is for the two modules (view and view model), over the 0.2 s the compiler needs with nothing to do. Growth is the exponent against the number of stations.

|                                   | 6 stations | 12 stations | 24 stations | 48 stations | growth |
| --------------------------------- | ---------- | ----------- | ----------- | ----------- | ------ |
| compile (s)                       | 0.3        | 0.7         | 2.1         | 8.6         | 1.61   |
| bundle, minified (KB)             | 499.6      | 520.8       | 593.5       | 857.9       | 0.25   |
| bundle, gzipped (KB)              | 95.6       | 97.2        | 100.6       | 108.4       | 0.06   |
| drawn (ms)                        | 61.6       | 72.23       | 79.71       | 96.36       | 0.21   |
| DOM nodes                         | 966        | 1356        | 2136        | 3696        | 0.65   |
| a tick fed, one station open (ms) | 1.41       | 1.45        | 1.6         | 1.88        | 0.14   |
| a button clicked (ms)             | 4.08       | 4.11        | 4.13        | 5.2         | 0.11   |

No exception and no console warning.

