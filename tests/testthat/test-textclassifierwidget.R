title <- "Automatic Categorization: What don't you like about Tom Cruise?  That is, what is it about Tom Cruise that you dislike?"

category.accuracy <- c(`Religion and Scientology` = 1, Nothing = 1, Ego = 1)

cv.metrics <- structure(c(50, 100, 111, 61, 0.8326531, 0.9186441, 0.7457670, 0.846862, 0.7806129, 0.9126599),
                        .Dim = c(2L, 5L),
                        .Dimnames = list(NULL,
                                         c("Estimation sample size", "Validation sample size",
                                           "Accuracy", "Kappa", "F1")))

footer <- "n = 299 cases were used in the text processing of a total of 300; cases containing missing values have been excluded; existing categorization classified 107 cases into 1 of 3 categories; training sample performance - Accuracy: 100.0%; Cohen's Kappa: 1.00; F1:1.00;"

missing <-rep(FALSE, 300)
missing[115] <- TRUE

observed.counts <- c(`Religion and Scientology` = 62, Nothing = 49, Ego = 22)
overall.metrics <- c(Accuracy = 1, Kappa = 1, F1 = 1)

text.raw.by.categorization <- structure(
    list(c(`4` = "RELIGOUS VIEWS", `14` = "scientology",
           `15` = "his scientology beliefs",
           `19` = "his apparant (whether real or not) arrogance; Scienetology",
           `23` = "Vain Scientologist", `24` = "He is a strange religious freak",
           `27` = "That he left Nicole Kidman.", `40` = "his ridiculous religion",
           `44` = "I like Tom Cruise over all but it seems like he isn't being totally honest about some things in his life.",
           `47` = "science christian", `54` = "I hate the Scientology thing.",
           `57` = "scientology", `59` = "Thinks he's GOD", `61` = "I just think the media makes him out to be crazy. The Scientology part is crazy",
           `64` = "His church!", `67` = "He is a little strange. I do not trust members of cults.",
           `70` = "his \"religion\"", `73` = "That he and his bitch ass church are allowed to treat people like shit.",
           `74` = "He's a scientologist nut.", `78` = "I think Tom Cruise is somewhat arrogant.",
           `86` = "his religion and he just thinks he is too good for everyone else.",
           `87` = "they way he treated his wife and child", `90` = "He's a control freak in a crazy religion",
           `94` = "he's a religious zealot/jerk", `96` = "he's a scientologist.",
           `98` = "He's too judgemental of others beliefs", `103` = "scientology",
           `106` = "I do not know enought about Tom Cruise to give an answer.",
           `112` = "I like Tom Cruise.", `113` = "I don't think he's a straightforward person; too involved in scientology",
           `114` = "I believe he is stuck up and takes advantage of women.",
           `118` = "I'm sorry that he is involved with Scientology however that is his business",
           `119` = "the fact that he don't do fantasy movies any more.",
           `122` = "scientology craziness", `123` = "nothing he is an idiot who is in a Scientology cult",
           `126` = "I like Tom Cruise. He's a decent actor but gets a lot of bad press for his religion which is stupid.",
           `131` = "his attitude his religion", `138` = "Too much of a show off and too visible with his Scientology.",
           `146` = "He is a brainwashed cult crazy lunatic with an intolerant agenda.",
           `149` = "He is a weirdo ever since the War of the World Movie",
           `167` = "I like TOm cruise", `173` = "his religion", `177` = "scientology",
           `181` = "He's sort of weird, and the fact that he divorced Nicole just before their 10th year anniversary was just low.",
           `185` = "hes just weird and a scientologist", `195` = "Good actor but not sure about his religious connection.",
           `199` = "He's a decent actor - his Scientology beliefs are nuts",
           `202` = "He thinks he knows more than other people because he is a Scientolgist",
           `206` = "i like tom cruise but he can have his moments he needs to break away from the crazy religion he is in and try to make it work with his ex wife to take care of his daughter",
           `208` = "his narrow views on other faiths", `212` = "his religious beliefs or sinintology practices",
           `214` = "that he's a Scientologist", `218` = "I think he is arrogant and has a weird religious philosophy.",
           `220` = "His particular faith which he appeared to force his ex wife to accept and live according to their rules.",
           `221` = "I like Tom cruise", `222` = "His Scientology is forced upon people. He also appears smug.",
           `224` = "that he is a christian scientist.", `231` = "He pushes his religion on people.",
           `233` = "maybe because he believe in scientogy", `234` = "Scientology",
           `235` = "He's a Scientologist.", `236` = "Other than his religious views, nothing.",
           `238` = "I respect the religion of eveyone ... But sciensology is enough...",
           `239` = "He's a crazy cult member who puts other people down for not thinking the way he does.",
           `242` = "His movies from the 90s and 80s. I dislike his psycho religion.",
           `248` = "He judges people and their lifestyle. for example when said bad things about Brooke shields because she took anti depressants after she had her child.",
           `249` = "scientology", `251` = "Scientology!", `252` = "Crazy man, religious beliefs.",
           `255` = "I LOVE TOM CRUIE DONT LIKE THE SCIENTOLOGY THING BUT LOVE IS ACTING",
           `259` = "I don't like that he's a controlling mysogynist, or his involvement with the church of scientology",
           `260` = "His religious affiliation in the last several years.",
           `266` = "His affiliation. With Scientology", `267` = "I don't care for his religious choices and how he treats his women.",
           `270` = "I like Tom cruise", `271` = "I adore Tom Cruise however; the Scientology issue is extremely worrisome and very odd.",
           `275` = "That he perches about Scientology to others is a dislike but I love his movies!!",
           `278` = "The way he keeps his children in tabloids and his religious.views being made into headline news",
           `279` = "He seems to be elusive about his religion.", `281` = "His religion is a little bizarre and I'm not a fan of any of his movies.",
           `284` = "Im not familiar with Tom Cruise", `291` = "i don't have an opinion on tom cruise",
           `293` = "The different religion he is fallowing", `294` = "I like all off tom cruise",
           `297` = "Tom Cruise is too cocky. It annoys me.", `299` = "Tom Cruise just isn't that interesting in this day and age.",
           `300` = "I dislike the way he changed over to Scientology and the drama with Katie Holmes and his self."
    ),
    c(`1` = "Nothing I really like him.", `5` = "Nothing", `11` = "nothing",
      `16` = "nothing", `18` = "everything", `20` = "everything", `21` = "nothing",
      `25` = "i like him and dislike nothing", `33` = "that he isnt here,nothing else.",
      `34` = "There is nothing about tom cruise i dislike.", `35` = "I really have nothing aganst him because he worked and earned money on his own merits'. On the other hand I hate Chelsea Clinton because she got handed high paying jobs' based on who her parents' are.",
      `39` = "just about everything", `43` = "nothing", `46` = "Nothing. He is ok!",
      `49` = "nothing", `53` = "absolutely NOTHING!!!!!!", `77` = "I don't think he is an honest person.",
      `80` = "nothing", `83` = "nothing i dislike", `92` = "nothing",
      `99` = "There isn't anything that I dislike about Tom Cruise.",
      `108` = "nothing", `109` = "nothing", `120` = "nothing", `123` = "nothing he is an idiot who is in a Scientology cult",
      `129` = "nothing", `132` = "nothing", `135` = "nothing", `139` = "nothing",
      `140` = "Nothing", `148` = "nothing", `151` = "nothing", `155` = "nothing",
      `156` = "Absolutely nothing.", `157` = "nothing", `160` = "nothing",
      `163` = "I like him, there nothing to dislike about him", `165` = "Pretty much everything!",
      `166` = "Nothing", `168` = "Nothing in particular", `176` = "Everything",
      `183` = "nothing i dislike", `189` = "Nothing", `190` = "nothing",
      `193` = "nothing nothing", `211` = "everything", `219` = "i like his good acting and boyish look nothing i don't like about T.C.",
      `223` = "Nothing", `232` = "Nothing dont know the guy.", `236` = "Other than his religious views, nothing.",
      `244` = "nothing", `253` = "Nothing really u can't take his great acting away from him , he's kida weird, but who r we to say",
      `256` = "Nothing at all, I happen to think that he is an egomaniac, although i did like his wife Katie Holmes and I love their little girl Sury!",
      `262` = "nothing", `263` = "nothing", `269` = "There is nothing about him that i don't like",
      `282` = "I like Tom cruise have nothing negative about him?",
      `288` = "I don't dislike anything about Tom Cruise"),
    c(`13` = "his ego",
      `26` = "he seems really weird and kinda crazy", `61` = "I just think the media makes him out to be crazy. The Scientology part is crazy",
      `82` = "He's arrogant enough to believe he is not crazy", `90` = "He's a control freak in a crazy religion",
      `141` = "Much to egotistical and standoffish, and full of himself.",
      `144` = "he is short and creepy", `146` = "He is a brainwashed cult crazy lunatic with an intolerant agenda.",
      `153` = "I think he's a little fanatical", `154` = "he is crazy",
      `158` = "His inflated ego.", `179` = "He can't act anymoreand his head is to big for his ego. And all he does this the same thing, seen one mission seen them all.",
      `181` = "He's sort of weird, and the fact that he divorced Nicole just before their 10th year anniversary was just low.",
      `186` = "Whether he actually is or not, his media persona is crazy, unhinged, controlling, and strange.",
      `194` = "he is crazy", `196` = "He's pretty egocentric, and I don't think he's a very good actor - pretty much all the characters he plays are the same.",
      `198` = "I think he's crazy", `200` = "He seems to get the STRANGEST publicity.",
      `201` = "He's crazy", `206` = "i like tom cruise but he can have his moments he needs to break away from the crazy religion he is in and try to make it work with his ex wife to take care of his daughter",
      `209` = "He seems crazy.", `216` = "he's crazy", `239` = "He's a crazy cult member who puts other people down for not thinking the way he does.",
      `252` = "Crazy man, religious beliefs.", `256` = "Nothing at all, I happen to think that he is an egomaniac, although i did like his wife Katie Holmes and I love their little girl Sury!",
      `276` = "That he's crazy"),
    c(`115` = NA_character_)),
    .Names = c("Religion and Scientology", "Nothing", "Ego", NA))

weighted.sizes <- structure(c(87L, 58L, 26L),
                            .Names = c("Religion and Scientology", "Nothing", "Ego"))

examples <- c(`299` = "Tom Cruise just isn't that interesting in this day and age.",
              `168` = "Nothing in particular", `252` = "Crazy man, religious beliefs.")

test_that("Widget output check", {
    expect_error(result <- TextClassifierWidget(observed.counts, weighted.sizes, category.accuracy,
                                                examples, cv.metrics = NULL, text.raw.by.categorization,
                                                missing, title, footer),
                 NA)
    expect_is(result, "htmlwidget")
    expect_is(result, "rhtmlMetro")
    expect_error(result <- TextClassifierWidget(observed.counts, weighted.sizes, category.accuracy,
                                                examples, cv.metrics = cv.metrics, text.raw.by.categorization,
                                                missing, title, footer),
                 NA)
    expect_is(result, "htmlwidget")
    expect_is(result, "rhtmlMetro")
})
