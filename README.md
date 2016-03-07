
Using Pandoc template for reveal.js: see https://gist.github.com/aaronwolen/5017084

Generate slides, needs local installation of reveal.js.

```
pandoc --slide-level 1 --variable theme=beige -i -s -o slides.html --template=template-revealjs.html -t revealjs slides.md && sed -i -e s/h1/h3/g slides.html
```
