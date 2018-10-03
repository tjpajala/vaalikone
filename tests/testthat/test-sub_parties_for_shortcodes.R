context("test-sub_parties_for_shortcodes")

test_that("subbing parties for shortcodes works", {
  short_parties<-c("IP","KA","KD","KESK","KOK","Other","KTP","M2011","PIR","PS","RKP","SDP","SKP","STP","VAS","VIHR","VP", "RKP")
  parties <- c("Itsenäisyyspuolue","Köyhien Asialla","Suomen Kristillisdemokraatit","Suomen Keskusta","Kansallinen Kokoomus","Other","Kommunistinen Työväenpuolue","Muutos 2011","Piraattipuolue","Perussuomalaiset","Suomen ruotsalainen kansanpuolue","Suomen Sosialidemokraattinen Puolue","Suomen Kommunistinen Puolue","Suomen Työväenpuolue STP","Vasemmistoliitto","Vihreä liitto","Vapauspuolue","Ruotsalainen kansanpuolue")
  
  expect_equal(sub_parties_for_shortcodes(parties), short_parties)
  expect_equal(sub_parties_for_shortcodes("Suomen Kes"), "Suomen Kes")
})

test_that("subbing a partial party string does not change anything", {
  expect_equal(sub_parties_for_shortcodes("Suomen Kes"), "Suomen Kes")
})