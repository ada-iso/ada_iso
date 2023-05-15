with ISO_Countries_Tests;
with ISO_Currencies_Tests;
procedure Iso_Tests is
begin
    ISO_Countries_Tests.Run_Tests;
    ISO_Currencies_Tests.Run_Tests;
end Iso_Tests;