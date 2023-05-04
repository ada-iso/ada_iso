# Generating an update
1. Consolidating the reference files
   - For Countries:
      1. Visit the [Country Code Search](https://www.iso.org/obp/ui/#search/code/)
      2. Be sure to set the page length to 300 to catch everything.
      3. Paste the table into a CSV
      4. **REMOVE** the French Short Name, column B.
      5. Verify the CSV Has the following headers in this exact order: `English short name,Alpha-2 code,Alpha-3 code,Numeric`
      6. Save the CSV in the `generator/files` folder.
2. Build the generate app
   1. `cd generate`
   2. `alr build`
3. Run the generate app
   1. `bin/generate`
   2. The source files will be stored in the `generate/output` directory.
4. Copy the source files to `ada_iso/src` (not `generate/src`!) folder.