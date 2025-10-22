# mcmodule 1.1.0

-   **Re-submission to CRAN**: Removed unexported function examples.

-   **Bug Fixes:** Fixed missing "agg_suffix" handling, improved dimension matching for aggregated nodes, resolved prefix issues in combined probability nodes.

-   **Feature Enhancements:** Added "match_keys" parameter to `eval_module()` for flexible data-mcnode matching, implemented multiple "prev_mcmodule" data names support in `eval_module()`, and improved R function handling within mcmodule expressions. Enhanced `trial_totals()` to support "agg_suffix" skipping, improved agg_keys combination with `at_least_one()`, and added NA removal in key columns

-   **Documentation:** Updated documentation and improved error messages for missing prev_nodes and mc_keys handling

-   **Testing**: Added tests for totals custom names and various dimension matching scenarios

# mcmodule 1.0.1

-   **Re-submission to CRAN**: Removed unexported function examples, replaced "dontrun" with "donttest", added vignette link to DESCRIPTION, and included citation file.

# mcmodule 1.0.0

-   Initial CRAN submission.
