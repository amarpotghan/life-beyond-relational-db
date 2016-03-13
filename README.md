
Using Pandoc template for reveal.js: see https://gist.github.com/aaronwolen/5017084

Generate slides, needs local installation of [reveal.js](https://github.com/hakimel/reveal.js)

```
pandoc --slide-level 2 --variable theme=beige -i -s -o slides.html --template=template-revealjs.html -t revealjs slides.md && sed -i -e s/h1/h3/g slides.html
```

Generate PDF slides (not very nice...)

``` 
pandoc --slide-level 2 -i -s -t beamer -o slides.pdf slides.md  --latex-engine=xelatex -V colortheme:beaver 
```
