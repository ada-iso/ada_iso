# Generating an update
1. Consolidating the reference files
   - For Countries:
      1. Visit the [Country Code Search](https://www.iso.org/obp/ui/#search/code/)
      2. Be sure to set the page length to 300 to catch everything.
      3. Paste the table into a CSV
      4. **REMOVE** the French Short Name, column B.
      5. Verify the CSV Has the following headers in this exact order: `English short name,Alpha-2 code,Alpha-3 code,Numeric`
      6. Save the CSV in the `generator/files` folder as `countries.csv`.
   - For Currencies:
      1. Visit the [ISO 4217 Standards Website](https://www.iso.org/iso-4217-currency-codes.html)
      2. Download the ISO standard in XML format.  Specifically `list-one.xml` and `list-three.xml`
         1. Rename `list-one.xml` as `currencies.xml`
         2. Rename `list-three.xml` as `currencies-historic.xml`
      3. Save these two _renamed_ xml files in `generator/files`
      4. If you have an update for currency symbols, I am manually maintaining this in `currencies-symbols.csv`.  So just edit that file.
      - NOTE: Ensure that country codes have already been downloaded. The generator relies on them.
2. Build the generate app
   1. `cd generate`
   2. `alr build`
3. Run the generate app
   1. `bin/generate`
   2. The source files will be stored in the `generate/output` directory.
4. Copy the source files to `ada_iso/src` (not `generate/src`!) folder.