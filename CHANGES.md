# CHANGES

## 0.1.5

No programmatic changes to 0.1.4. The 0.1.4 release did not have the pack version match the cli.pl version. Fixed by incrementing to 0.1.5

## 0.1.4

 * Fixed README lines about .DELETE_ON_ERROR - see #46
 * Fixes #52
 * Resolves #50 implementing 'export' with tests
 * Resolves #46 and adds tests
 * Fixes #52
 * Fixes #44 with test
 * Fixes #49 with test

## 0.1.2

 * If errors are ignored, report this. This happens if an exec fails, and either -k is passed or the line starts '-'
 * fixed erroneous conversion of recursively-expanded variables into simply-expanded variables when appending, added test
 * Always report job submission
 * Implemented MAKECMDGOALS special variable, added test. Closes #39
 * evaluating arguments for $(bagof T,G). Fixes issue #37
 * fixes for #35 and #36
 * Fix to prevent over-eager consumption of backslashes as part of target, fixes #33
 * Declare debug topics in advance #31
 * Added more efficient solution to merging duplicate variables due to @triska in comment on cf75abd
 * Added support for .IGNORE
 * Added support for .PHONY, and tests for .PHONY and .ONESHELL
 * braces in deplists were being misinterpreted as goals by parser, and substitution references weren't working on lists - both fixed
 * Handle quoted arguments correctly in wrapper script. Fixes #28

## 0.1.1

 * Translate terms in prolog/endprolog block with expand_term/2 before asserting them. Fixes #21
 * renamed swipl wrapper script. fixes #22

## 0.1.0

This release incorporates major changes from v0.0.3, contributed by @ihh, including multiple new features, including:

 * triggering builds based off changes to MD5 signatures
 * Support for cluster-based job queues.
 * Support for GNU-Make syntax as an alternative to prolog syntax (and the default)
 
Many command line options may have changed, see README for details

## 0.0.3

## 0.0.2



