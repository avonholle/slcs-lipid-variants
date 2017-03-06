
# docx example ---------
doc.filename = "ex_paragraph.docx"
doc <- docx()
styles(doc)

doc = addTitle( doc, "Title example 1", level = 1 )

# Add "Hello World" into the document doc
doc = addParagraph(doc, "Hello Word", stylename = "Normal" )

doc = addTitle( doc, "Title example 2", level = 1 )

# define some text
sometext = c( "Lorem ipsum dolor sit amet, consectetur adipiscing elit.", 
              "In sit amet ipsum tellus. Vivamus dignissim arcu sit amet faucibus auctor.", 
              "Quisque dictum tristique ligula.")

# add sometext with stylename BulletList
doc = addParagraph( doc, value = sometext, stylename="BulletList" )

doc = addTitle( doc, "Title example 3", level = 1 )

# "My tailor is rich" with formatting on some words
pot1 = pot("My tailor", textProperties(color = "red" ) 
) + " is " + pot("rich", textProperties(shading.color = "red", 
                                        font.weight = "bold" ) )

# "Cats and dogs" with formatting on some words
pot2 = pot("Cats", textProperties(color = "red" ) ) + 
  " and " + 
  pot("dogs", textProperties( color = "blue" ), 
      hyperlink = "http://www.wikipedia.org/" )

# create a set of paragraphs made of pot1 and pot2
my.pars = set_of_paragraphs( pot1, pot2 )

# Add my.pars into the document doc
doc = addParagraph(doc, my.pars, stylename = "Normal" )

doc = addTitle( doc, "Title example 4", level = 1 )
# define some text
text1 = "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
text2 = "In sit amet ipsum tellus. Vivamus arcu sit amet faucibus auctor."
text3 = "Quisque dictum tristique ligula."

# define parProperties with list properties
ordered.list.level1 = parProperties(list.style = "ordered", 
                                    level = 1 )
ordered.list.level2 = parProperties(list.style = "ordered", 
                                    level = 2 )

# define parProperties with list properties
unordered.list.level1 = parProperties(list.style = "unordered", 
                                      level = 1 )
unordered.list.level2 = parProperties(list.style = "unordered", 
                                      level = 2 )

# add ordered list items 
doc = addParagraph( doc, value = text1, 
                    par.properties = ordered.list.level1 )
doc = addParagraph( doc, value = text2, 
                    par.properties = ordered.list.level2 )

# add ordered list items without restart renumbering
doc = addParagraph( doc, value = c( text1, text2, text3), 
                    par.properties = ordered.list.level1 )

# add ordered list items and restart renumbering
doc = addParagraph( doc, value = c( text1, text2, text3), 
                    restart.numbering = TRUE, par.properties = ordered.list.level1 )

# add unordered list items
doc = addParagraph( doc, value = text1, 
                    par.properties = unordered.list.level1 )
doc = addParagraph( doc, value = text2, 
                    par.properties = unordered.list.level2 )


writeDoc( doc, file = doc.filename )

# pptx example -------
doc.filename = "ex_paragraph.pptx"

options( "ReporteRs-fontsize" = 24 )
doc = pptx( title = "title" )

# add a slide with layout "Title and Content"
doc = addSlide( doc, slide.layout = "Title and Content" )

doc = addTitle( doc, "Title example 1" )

# Add "Hello World" into the document doc
doc = addParagraph(doc, "Hello Word" )

# add a slide with layout "Title and Content"
doc = addSlide( doc, slide.layout = "Title and Content" )

doc = addTitle( doc, "Title example 2" )

# "My tailor is rich" with formatting on some words
pot1 = pot("My tailor", textProperties(color = "red" ) 
) + " is " + pot("rich", textProperties(shading.color = "red", 
                                        font.weight = "bold" ) )

# "Cats and dogs" with formatting on some words
pot2 = pot("Cats", textProperties(color = "red" ) ) + 
  " and " + 
  pot("dogs", textProperties( color = "blue" ), 
      hyperlink = "http://www.wikipedia.org/" )

# create a set of paragraphs made of pot1 and pot2
my.pars = set_of_paragraphs( pot1, pot2 )

# Add my.pars into the document doc
doc = addParagraph(doc, my.pars )

# Add my.pars into the document doc
doc = addParagraph(doc, my.pars, offx = 3, offy = 3, 
                   width = 2, height = 0.5,
                   par.properties=parProperties(text.align="center", padding=0) )

# add a slide with layout "Title and Content"
doc = addSlide( doc, slide.layout = "Title and Content" )

doc = addTitle( doc, "Title example 3" )

# "My tailor is rich" with formatting on some words
pot1 = pot("My tailor", textProperties(color = "red" ) 
) + " is " + pot("rich", textProperties(shading.color = "red", 
                                        font.weight = "bold" ) )

# "Cats and dogs" with formatting on some words
pot2 = pot("Cats", textProperties(color = "red" ) ) + 
  " and " + 
  pot("dogs", textProperties( color = "blue" ), 
      hyperlink = "http://www.wikipedia.org/" )

# create a set of paragraphs made of pot1 and pot2
my.pars = set_of_paragraphs( pot1, pot2 )

# Add my.pars into the document doc
doc = addParagraph(doc, my.pars, 
                   par.properties=parProperties(text.align="center", padding=24) )

# add a slide with layout "Title and Content"
doc = addSlide( doc, slide.layout = "Title and Content" )

doc = addTitle( doc, "Title example 1" )
# define some text
text1 = "Lorem ipsum dolor sit amet, consectetur adipiscing elit."
text2 = "In sit amet ipsum tellus. Vivamus arcu sit faucibus auctor."
text3 = "Quisque dictum tristique ligula."

# define parProperties with list properties
ordered.list.level1 = parProperties(list.style = "ordered", 
                                    level = 1 )
ordered.list.level2 = parProperties(list.style = "ordered", 
                                    level = 2 )

# define parProperties with list properties
unordered.list.level1 = parProperties(list.style = "unordered", 
                                      level = 1 )
unordered.list.level2 = parProperties(list.style = "unordered", 
                                      level = 2 )

# add ordered list items 
doc = addParagraph( doc, value = text1, 
                    par.properties = ordered.list.level1 )
doc = addParagraph( doc, value = text2, append = TRUE, 
                    par.properties = ordered.list.level2 )

doc = addParagraph(doc, "This paragraph has no list attribute", 
                   append = TRUE )

# add ordered list items without restart renumbering
doc = addParagraph( doc, value = c( text1, text2, text3), 
                    append = TRUE, par.properties = ordered.list.level1 )

# add ordered list items and restart renumbering
doc = addParagraph( doc, value = c( text1, text2, text3), 
                    append = TRUE, restart.numbering = TRUE, 
                    par.properties = ordered.list.level1 )

# add unordered list items
doc = addParagraph( doc, value = text1, 
                    append = TRUE, 
                    par.properties = unordered.list.level1 )
doc = addParagraph( doc, value = text2, 
                    append = TRUE, 
                    par.properties = unordered.list.level2 )

getwd()
writeDoc( doc, file = doc.filename )
