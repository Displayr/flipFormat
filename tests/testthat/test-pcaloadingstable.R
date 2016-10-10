context("PCALoadingsTable")

loadings <- structure(c(0.273822297037884, 0.549763211142948, 0.569592524162743,
                        0.35704232365874, 0.280329151054858, -0.0219964312917683, 0.205396546966957,
                        0.0334145205393334, 0.107069124344958, 0.399128612219069, 0.154562187402283,
                        -0.188773117158305, 0.464027113478339, -0.085295400212996, -0.37541599069534,
                        0.431719803270152, 0.108299343424544, 0.339509047617505, -0.409439709102214,
                        0.549561468250643, 0.720806670976849, 0.768920824033285, 0.774912999661358,
                        0.312584129711437, 0.550437742859484, -0.254969374870284, 0.112837818733791,
                        -0.0662763176069618, 0.011618753080747, 0.241161387955513, 0.285806417503329,
                        0.424920054285117, 0.541389805089761, 0.498014613343547, -0.150372112652334,
                        0.0561169989241544, 0.619903215735556, -0.174333474994343, 0.300415866733973,
                        0.619942856671702, -0.329856628412289, 0.100207377099242, -0.268864931872416,
                        0.296709294209964, 0.244301186924714, 0.233897362973193, 0.165572727879258,
                        0.21382055899204, 0.149555228757614, 0.0557951782743539), .Dim = c(25L,
                                                                                           2L), .Dimnames = list(c("q23 20% Missing: Allows to keep in touch ",
                                                                                                                   "q23 20% Missing: Technology fascinating ", "q23 20% Missing: Like look of phones ",
                                                                                                                   "q23 20% Missing: Surprised by bill size ", "q23 20% Missing: Stops other people monitoring ",
                                                                                                                   "q23 20% Missing: Difficult to determine best deal ", "q23 20% Missing: Spent a lot of time shopping for best deal ",
                                                                                                                   "q23 20% Missing: Closely monitors time on phone ", "q23 20% Missing: Cost is a factor when deciding to SMS or phone ",
                                                                                                                   "q23 20% Missing: All friends have mobiles ", "q23 20% Missing: Large phones mean no image ",
                                                                                                                   "q23 20% Missing: Calls kept short and to the point ", "q23 20% Missing: Important to be able to contact friends whenever ",
                                                                                                                   "q23 20% Missing: Email used more than mobile ", "q23 20% Missing: Only use mobile for essential calls ",
                                                                                                                   "q23 20% Missing: Mobile an essential part of lifestyle ", "q23 20% Missing: Mobile assists personal safety ",
                                                                                                                   "q23 20% Missing: Can be contacted whenever needed ", "q23 20% Missing: Only basic functions used ",
                                                                                                                   "q23 20% Missing: Like fast internet access on phone ", "q23 20% Missing: Would like mobiles to work as video phones ",
                                                                                                                   "q23 20% Missing: Would like mobiles to download and play music ",
                                                                                                                   "q23 20% Missing: Would like mobiles to download and view short videos ",
                                                                                                                   "q23 20% Missing: More likely to change companies in the future due to number portability ",
                                                                                                                   "q23 20% Missing: Would like to do mobile banking with phone "
                                                                                           ), c("Component 1", "Component 2")))

variance.explained <- c(0.177613979888284, 0.0940391993551916)
eigenvalues <- c(4.44034949720709, 2.35097998387979)

expect_error(PCALoadingsTable(loadings, variance.explained, eigenvalues, 0.2, title = "Principal Component Loadings",
    footer = "Loadings; Input: Correlation matrix; Missing data setting: Use partial data (pairwise correlations); Sample size: 4 to 618; Rotation: None"), NA)


expect_error(PCALoadingsTable(loadings, NULL, eigenvalues, 0.2, title = "Principal Component Structure",
                              footer = "Structure matrix; Input: Correlation matrix; Missing data setting: Use partial data (pairwise correlations); Sample size: 4 to 618; Rotation: None"), NA)
