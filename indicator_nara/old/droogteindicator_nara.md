---
title: "Ontwerp van een early-warning meetnet voor droogte in natuurgebieden"
author: "Jan Wouters, Floris Vanderhaeghe"
date: 'Versie 2020-07-09 12:03:29'
link-citations: true
bibliography: lit_droogtemeetnet.bib
csl: research-institute-for-nature-and-forest.csl
linkcolor: link.colour
citecolor: link.colour
urlcolor: link.colour
geometry: margin=1in
mainfont: "Calibri"
fontsize: 11pt
always_allow_html: yes #mogelijk slechts een tijdelijke verplichting
# documentclass: "article"
site: bookdown::bookdown_site
documentclass: book
params:
  refresh_data: 2
  refresh_figures: 2
output: 
  # bookdown::gitbook: default
    #bookdown::pdf_book: default
  # number_sections: TRUE
  bookdown::html_document2:
    keep_md: TRUE
    number_sections: yes
    fig_caption: yes
    df_print: paged
    toc: TRUE
    toc_float:
      collapsed: FALSE
      smooth_scroll: FALSE
    toc_depth: 4
    includes:
        in_header: header.html
  bookdown::pdf_document2:
    fig_caption: yes
    keep_tex: yes
    toc: yes
    toc_depth: 3
    latex_engine: xelatex
    number_sections: true
    includes:
        in_header: header.tex
editor_options: 
  chunk_output_type: console
---

<!--chapter:end:index.Rmd-->

# Inleiding

> Het basisidee van het early-warning meetnet droogte is om in natuurgebieden een vooraf gekozen aantal meetpunten zo goed mogelijk ruimtelijk gebalanceerd te selecteren, rekening houdende met mogelijke verschillen in het gedrag van het grond/oppervlaktewaterpeil t.a.v. een neerslagtekort. Voor deze meetpunten kan op basis van actuele informatie een indicator (cfr. droogte-indicator) worden berekend.

Het project omhelst : 

* uitwerking van een methodiek die een ruimtelijk gebalanceerde selectie van actuele en potentiële meetlocaties mogelijk maakt 
* bepalen van referentiewaarden voor de actuele meetlocaties

Met natuurgebieden worden hier alle locaties met grondwaterafhankelijke habitattypes en rbb's bedoeld, gelegen binnen of buiten het Natura 2000 netwerk. In overeenstemming met het meetnet natuurlijk milieu (MNM) voor de milieudruk 'verdroging via grondwater' worden volgende habitattypen en rbb's als grondwaterafhankelijk beschouwd (tabel <!--\@ref(tab:gw-typen)-->). Hierna worden deze typen afgekort tot gaHT. 


  

Een min of meer ruimtelijk gebalanceerde set van bestaande meetlocaties maakt het mogelijk om conclusies te trekken die benaderend representatief zijn voor heel het studiegebied.
Via deze benadering wordt ook een goede synergie bekomen met de implementatie van de zg. 'overgangsfase' van het grondwatermeetnet in MNM [@vanderhaeghe_meetnetten_2018: hoofdstuk 6].

Er wordt met dit meetnet niet beoogd om specifiek voor elk gaHT te bepalen of het aan droogte onderhevig is. Het meetnet wil een globale uitspraak faciliteren.
Hierbij achten we het belangrijk om rekening te houden met de mogelijke verschillen in responstijd ('hoe snel laat een neerslagtekort zich voelen in een wijziging van het grondwaterpeil') op droogte. De responstijd is onder meer afhankelijk van de landschappelijke positie. Vegetaties die voorkomen in lokaal-grondwatergevoede systemen zullen sneller een droogte gewaar worden dan vegetaties die gebonden zijn aan (regionale) systemen met een permanente aanvoer van grondwater.
We veronderstellen hierbij dat er een relatie bestaat tussen het hydrologisch regime en de responstijd: permanent grondwatergevoede systemen reageren trager dan systemen met een tijdelijke of zwakke grondwatervoeding. 

<!--We vertrekken hiervoor van de 5-delige indeling in grondwatertypegroepen (GT-groep) die toegepast wordt in het MNM (tabel \@ref(tab:GTgroepen)). In de vorige tabel \@ref(tab:gw-typen)) is weergegeven tot welke GT-groep een gaHT gerekend wordt.-->

    

De GT-groepen lenen zich heel goed om een stratificatie uit te voeren op basis van de verwachte responssnelheid van een locatie op droogte. 
De permanent gevoede locaties (GT-groep 1) zullen naar verwachting trager reageren dan de tijdelijk of zwak gevoede locaties. 
GT-groep 5 bundelt grondwaterafhankelijke HT die zodanig breed gedefinieerd zijn dat ze op sommige locaties grondwatergevoed zijn, maar soms tot meestal niet.
Het voordeel van deze groep is dat ze vermoedelijk het snelst op droogte zal reageren.
Ze houdt echter ook nadelen in, nl. dat je bij het uitzetten van een nieuw punt niet op voorhand weet of het wel grondwaterafhankelijk is en dat sommige typen heel variabel zijn m.b.t. het grondwaterregime.
We oordelen dat deze nadelen groter zijn dan het mogelijke voordeel van een snelle signalering.
De GT-groep 5 wordt daarom niet meegenomen in de verdere analyse.

Door een stratificatie over de GT-groepen toe te passen kan een balans worden verzekerd. 
Een stratificatie laat toe om een globale uitspraak te doen.
We raden echter ook aan in de beoordeling elke GT-groep apart mee te nemen, omdat ze verschillende signalen zullen geven. 



<!--chapter:end:01_Inleiding.Rmd-->

