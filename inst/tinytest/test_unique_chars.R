expect_true(unique_characters('abc'))
expect_true(unique_characters(' '))
expect_true(unique_characters(''))

expect_true(all(unique_characters(c('abc', ' ', ''))))

expect_false(unique_characters('aa'))
expect_false(unique_characters('a a'))
expect_false(unique_characters('abba'))

expect_false(all(unique_characters(c('aa', 'a a', 'abba'))))
