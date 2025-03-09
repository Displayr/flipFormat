{ pkgs ? import <nixpkgs> {}, displayrUtils }:

pkgs.rPackages.buildRPackage {
  name = "flipFormat";
  version = displayrUtils.extractRVersion (builtins.readFile ./DESCRIPTION); 
  src = ./.;
  description = ''Formatting to be used in print statements and other outputs. E.g.,
    number formatting, formatting of tables.'';
  propagatedBuildInputs = with pkgs.rPackages; [ 
    xml2
    janitor
    sparkline
    htmlwidgets
    DT
    colorspace
    httr
    rmarkdown
    rhtmlMetro
    flipU
    stringr
    stringi
    formattable
    htmltools
    knitr
    striprtf
  ];
}
