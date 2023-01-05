library(LAMRSAControl)
## 1
ob <- event_dataframe(event = "extTrans",
            time = 1,
            node = 1,
            dest = 1,
            n = 1,
            proportion = 0,
            select = 1,
            shift = 1)
expected <- structure(list(event = structure(1L, .Label = "extTrans", class = "factor"),
                           time = 1L, node = 1L, dest = 1L, n = 1L, proportion = 0,
                           select = 1L, shift = 1L),
                      class = "data.frame", row.names = c(NA, -1L))
stopifnot(identical(ob, expected))

## 2
ob <- tools::assertError(event_dataframe(event = "extTrans",
                         time = 1,
                         node = 1,
                         dest = 1,
                         n = 1,
                         proportion = 1,
                         select = 1,
                         shift = 1))[[1]]$message

stopifnot(identical(ob, "You should declare proportion 0 if n > 0"))

## 3
ob <- event_dataframe(event = "extTrans",
                      time = c(1, 2),
                      node = 1,
                      dest = 1,
                      n = 1,
                      proportion = 0,
                      select = 1,
                      shift = 1)
stopifnot(nrow(ob) == 2L)

## 4
ob <- tools::assertError(event_dataframe(event = "extTrans",
                         time = 1,
                         node = "a",
                         dest = 1,
                         n = 1,
                         proportion = 0,
                         select = 1,
                         shift = 1))[[2]]$message
stopifnot(identical(ob, "all(!is.na(node)) is not TRUE"))

## 5
ob <- tools::assertError(event_dataframe(event = "extTrans",
                         time = 1,
                         node = 1,
                         dest = 1,
                         n = 1,
                         proportion = 0,
                         select = 1,
                         shift = 7))[[1]]$message
stopifnot(identical(ob, "all(shift %in% c(0L, 1L, 2L, 3L, 4L, 5L)) is not TRUE"))

## 6
ob <- tools::assertError(event_dataframe(event = "foo",
                                         time = 1,
                                         node = 1,
                                         dest = 1,
                                         n = 1,
                                         proportion = 0,
                                         select = 1,
                                         shift = 1))[[1]]$message
stopifnot(all(grepl("should be one of", ob)))

## 7
ob <- event(type = 1,
            time = 1,
            node = 1,
            dest = 1,
            n = 1,
            proportion = 0)
expected <- structure(list(event = structure(1L, .Label = "extTrans",
                                             class = "factor"),
                           time = 1L,
                           node = 1L,
                           dest = 1L,
                           n = 1L,
                           proportion = 0,
                           select = 1L,
                           shift = 0L),
                      class = "data.frame", row.names = c(NA, -1L))
stopifnot(identical(ob, expected))

## 8
ob <- tools::assertError(event(type = 32,
                         time = 1,
                         node = 1,
                         dest = 1,
                         n = 1,
                         proportion = 0))[[1]]$message
stopifnot(identical(ob,
                    "all(type %in% df$type) is not TRUE"))
