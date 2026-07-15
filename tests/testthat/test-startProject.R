testthat::context("startProject")

if (!"startProject" %in% loadedNamespaces()) {
  for (file in c(
    "R/makeSnapshot.R",
    "R/makeMemoTemplate.R",
    "R/makeRTemplate.R",
    "R/makeRmdTemplate.R",
    "R/makeSasTemplate.R",
    "R/startProject.R"
  )) {
    source(file.path("../..", file), local = FALSE)
  }
}

testthat::test_that("startProject works", {
    tmpdir <- tempfile("startProject-legacy-")
    dir.create(tmpdir)
    on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

    startProject(main.dir = tmpdir,
                 proj.name = "test_proj", client = "John Smith, MD",
                 client.dept = "QA", main.statistician = "Rocio Lopez",
                 stats.collab = "Susana Arrigain, Anne Tang",
                 subfolders = "communications, data, graphs, memo, orig_data, others, r, sas, temp",
                 templates = "memo, sas, R, Rmd")
})

testthat::test_that("template.layout is deprecated but still works as a fallback", {
  tmpdir <- tempfile("deprecated-template-layout-")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  expect_warning(
    startProject(
      main.dir = tmpdir,
      proj.name = "deprecated_layout_test",
      templates = "R, sas",
      template.layout = "multi"
    ),
    "deprecated"
  )
})

testthat::test_that("makeSnapshot can be called directly", {
  tmpdir <- tempfile("makeSnapshot-direct-")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  makeSnapshot(
    project.dir = tmpdir,
    proj.name = "direct_snapshot",
    proj.num = "456",
    start.date = "January 15, 2025",
    templates = "memo"
  )

  snapshot_dir <- file.path(tmpdir, "direct_snapshot", "analysis_20250115")
  expect_true(dir.exists(snapshot_dir))
  expect_true(dir.exists(file.path(snapshot_dir, "memo")))
})

testthat::test_that("makeSnapshot accepts custom analysis subfolders", {
  tmpdir <- tempfile("makeSnapshot-custom-folders-")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  makeSnapshot(
    project.dir = tmpdir,
    proj.name = "custom_snapshot",
    proj.num = "789",
    start.date = "January 15, 2025",
    templates = "memo",
    analysis.subfolders = c("custom_a", "custom_b")
  )

  snapshot_dir <- file.path(tmpdir, "custom_snapshot", "analysis_20250115")
  expect_true(dir.exists(file.path(snapshot_dir, "custom_a")))
  expect_true(dir.exists(file.path(snapshot_dir, "custom_b")))
})

testthat::test_that("makeSasTemplate supports multi-file layout and simple headers", {
  tmpdir <- tempfile("makeSasTemplate-multi-")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  makeSasTemplate(
    sas.dir = tmpdir,
    proj.name = "demo_project",
    start.date = "January 15, 2025",
    version = "2",
    client = "Client Name",
    client.dept = "Clinical Development",
    main.statistician = "Jane Doe",
    stats.collab = "John Smith",
    sas.purpose = "Defines libraries and global parameters.",
    sas.notes = "See root README.md for versioning, dependencies, and execution order.",
    template.layout = "multi",
    sas.header.style = "simple"
  )

  expect_true(file.exists(file.path(tmpdir, "00_setup.sas")))
  expect_true(file.exists(file.path(tmpdir, "01_data_management.sas")))
  expect_true(file.exists(file.path(tmpdir, "03_analysis.sas")))

  contents <- readLines(file.path(tmpdir, "00_setup.sas"))
  expect_true(any(grepl("FILE:", contents)))
  expect_true(any(grepl("AUTHOR:", contents)))
  expect_true(any(grepl("PROJECT:", contents)))
  expect_true(any(grepl("PURPOSE:", contents)))
  expect_true(any(grepl("NOTES:", contents)))
})

testthat::test_that("R and SAS can use independent layouts and headers", {
  tmpdir <- tempfile("mixed-layouts-")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  makeSasTemplate(
    sas.dir = tmpdir,
    proj.name = "demo_project",
    start.date = "January 15, 2025",
    version = "2",
    client = "Client Name",
    client.dept = "Clinical Development",
    main.statistician = "Jane Doe",
    stats.collab = "John Smith",
    sas.purpose = "Defines libraries and global parameters.",
    sas.notes = "See root README.md for versioning, dependencies, and execution order.",
    template.layout = "single",
    sas.header.style = "default"
  )

  makeRTemplate(
    r.dir = tmpdir,
    r.name = "demo_r",
    proj.name = "demo_project",
    start.date = "January 15, 2025",
    version = "2",
    client = "Client Name",
    client.dept = "Clinical Development",
    main.statistician = "Jane Doe",
    stats.collab = "John Smith",
    r.purpose = "Performs the analysis.",
    r.notes = "See root README.md for versioning, dependencies, and execution order.",
    template.layout = "multi",
    r.header.style = "simple"
  )

  expect_true(file.exists(file.path(tmpdir, "00_setup.R")))
  expect_true(file.exists(file.path(tmpdir, "01_data_management.R")))
  expect_true(file.exists(file.path(tmpdir, "03_analysis.R")))

  r_contents <- readLines(file.path(tmpdir, "00_setup.R"))
  expect_true(any(grepl("FILE:", r_contents)))
  expect_true(any(grepl("PURPOSE:", r_contents)))
})

testthat::test_that("snapshot structure creates the expected folders and templates", {
  tmpdir <- tempfile("startProject-snapshot-")
  dir.create(tmpdir)
  on.exit(unlink(tmpdir, recursive = TRUE), add = TRUE)

  startProject(
    main.dir = tmpdir,
    proj.name = "snapshot_test",
    proj.num = "123",
    start.date = "January 15, 2025",
    structure = "snapshot",
    templates = "memo, sas, R, Rmd",
    subfolders = "communications, data, graphs, memo, orig_data, others, r, sas, temp",
    sas.name = "snapshot_analysis",
    template.layout = "multi",
    sas.header.style = "simple"
  )

  project_dir <- file.path(tmpdir, "snapshot_test")
  snapshot_dirs <- list.files(project_dir, pattern = "^analysis_", full.names = TRUE)

  expect_true(dir.exists(project_dir))
  expect_true(dir.exists(file.path(project_dir, "00_protocol_and_irb")))
  expect_true(dir.exists(file.path(project_dir, "01_data_specs")))
  expect_true(dir.exists(file.path(project_dir, "02_docs")))
  expect_true(dir.exists(file.path(project_dir, "03_include")))
  expect_true(dir.exists(file.path(project_dir, "04_manuscript")))
  expect_true(dir.exists(file.path(project_dir, "05_presentations")))
  expect_true(dir.exists(file.path(project_dir, "06_external_resources")))
  expect_true(file.exists(file.path(project_dir, "README.md")))
  expect_false(dir.exists(file.path(project_dir, "communications")))
  expect_false(dir.exists(file.path(project_dir, "memo")))
  expect_true(length(snapshot_dirs) == 1)
  expect_true(dir.exists(file.path(snapshot_dirs[1], "code")))
  expect_true(file.exists(file.path(snapshot_dirs[1], "code", "00_setup.sas")))
  expect_true(file.exists(file.path(snapshot_dirs[1], "code", "01_data_management.sas")))
  expect_true(file.exists(file.path(snapshot_dirs[1], "code", "03_analysis.sas")))
  expect_false(file.exists(file.path(snapshot_dirs[1], "code", "snapshot_analysis.sas")))
  expect_true(any(grepl("analysis_20250115", snapshot_dirs)))
})
