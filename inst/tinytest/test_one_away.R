expect_true(one_away('a', 'a'))
expect_true(one_away('aa', 'aa'))
expect_true(one_away('aaa', 'aaa'))

expect_true(one_away('pales', 'pale'))
expect_true(one_away('pale', 'bale'))
expect_true(one_away('pale', 'pales'))
expect_true(one_away('', ''))

expect_false(one_away('bale', 'pales'))
expect_false(one_away('bake', 'pale'))
