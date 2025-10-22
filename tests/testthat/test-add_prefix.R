test_that("add_prefix works", {
  old_names<-names(imports_mcmodule$node_list)
  imports_mcmodule_new <- add_prefix(imports_mcmodule)
  new_names<-names(imports_mcmodule_new$node_list)
  expect_equal(paste0("imports_mcmodule_",old_names), new_names)

})
