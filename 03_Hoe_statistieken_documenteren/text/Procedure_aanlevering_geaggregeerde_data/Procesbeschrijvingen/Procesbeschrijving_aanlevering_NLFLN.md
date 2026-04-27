---
output:
  word_document:
    reference_docx: "template.docx"
title: "Procedure aanlevering NKREDNEM (Aantal kredietnemers)"    
---



# Versies

| Versie | Auteur        | Aanpassing                              |
| -----: | ------------- | --------------------------------------- |
| 1      | Jorre & Bart  | Eerste versie versie in eerste template |









# Algemene informatie




## Het emailadres van VSA-medewerker die de data verzamelt en doorstuurt

bart.delbeke@vlaanderen.be



## Uitgebreide omschrijving van de inhoud van de data-aanlevering:

Het bestand bevat aantal mensen 
- die leefloon ontvangen (Het leefloon vormt een minimuminkomen voor mensen die niet over voldoende bestaansmiddelen beschikken.)
- die equivalent leefloon ontvangen ( Het equivalent leefloon is er voor mensen die niet in aanmerking komen voor het leefloon maar die zich in een vergelijkbare noodsituatie bevinden. In de praktijk wordt het vooral uitgekeerd aan personen die een verzoek hebben ingediend voor internationale bescherming (asielzoekers) en aan vreemdelingen die niet in het bevolkingsregister zijn ingeschreven.)

De aantallen worden gegeven per
- jaar (uittreksel in januari)
- Gemeente volgens NIS-code
- geslacht
- leeftijdscategorie
- huishoudtype


## Oplijsting van de doelen van de data-aanlevering:

- Aanmaak van VOS 163: Sociale bijstand
- Aanmaak van GSM-indicator AR_13	Personen met (equivalent) leefloon "Aantal personen met een leefloon of equivalent leefloon ten opzichte van de totale bevolking, begin van het jaar. Het leefloon kadert in het Recht op Maatschappelijke Integratie (RMI), het equivalent leefloon in het Recht op Maatschappelijke Hulp (RMH). Het RMH is er voor mensen die niet in aanmerking komen voor het RMI maar die zich in een vergelijkbare noodsituatie bevinden. Het (equivalent) leefloon is een minimuminkomen voor personen die niet over voldoende bestaansmiddelen beschikken. Het bedrag van het (equivalent) leefloon hangt af van de woonsituatie. Er bestaan in dit verband drie categorieën: personen die alleen wonen, personen die samenwonen en personen met een gezin ten laste met minstens één minderjarig kind. Het bedrag van het leefloon wordt verminderd met de inkomsten waarover de aanvrager zelf kan beschikken."




## Verwachte dimensies en onderverdelingen in de data:

- Per kalenderjaar
- Per Vlaamse gemeente volgens NIS-code en voor de drie gewesten
- Per geslacht
- Per Leeftijdsklasse
- Per huishoudtype 
- Per type: leefloon versus equivalent leefloon



## Noodzakelijke aanleverfrequentie van de data: 

jaarlijks



## Afspraak over de structuur van de bestandsnaam van de aangeleverde data: 

De data worden aangeleverd als xlsx bestand met naam “Equivalent_leefloon_YYYY01-YYYY01.xlsx” waarbij YYYY wordt vervangen door het begin- en eindjaar van de aanlevering.











# Stap 1: verzamelen van de ruwe data



## bron/producent van de cijfers: 

Programmatorische Overheidsdienst (POD) Maatschappelijke Integratie, Federale Pensioendienst (FPD) & Directie-Generaal (DG) Handicap van Federale Overheidsdienst (FOD) Sociale Zekerheid, bewerking Statistiek Vlaanderen



## Weblink naar de producent of de webpagina waar de data werden gedownload:

https://www.mi-is.be/nl



## Beschrijving van het verzamelingsproces: 

De POD MI stuurt elk jaar automatisch de data van het laatste jaar door naar bart.delbeke@vlaanderen.be met een vaste structuur. Dit is geen nuttig proces omdat data uit voorgaande jaren nog kan wijzigen waardoor het risico bestaat dat we bij de VSA verouderde data bewaren. Ieder jaar moeten dus alle data vanaf begin van metingen (2005) opnieuw worden opgevraagd.  

Data worden aangevraagd via mail naar aanspreekpunt bij POD MI: Frederic.Swaelens@mi-is.be of vraag@mi-is.be.
De data worden in vaste structuur aangeleverd als zip-bestand in bijlage per mail.





## Beschrijving van hoe de dataleverancier/bron de cijfers verzamelde: 


De gegevens zijn gebaseerd op de terugbetalingsaanvragen die de Openbare Centra voor Maatschappelijk Welzijn (OCMW’s) maandelijks aan de POD MI bezorgen.





















# Stap 2: transformatie naar gekuiste data en validatie



## Over welke geografische eenheden is er informatie aanwezig in de ruwe dataset. Waar is de informatie over de geografische eenheden te vinden in de dataset. Als de dataset geen expliciete NIS-codes, postcodes of andere geografische codes bevat in overeenstemming met de codelijsten, hoe moeten die dan worden afgeleid? 

In het ruwe bestand hebben we informatie over NIS-codes volgens de NIS5 indeling van 01/01/2019 voor alle Belgische gemeenten in kolom 'Code CPAS INS (cd)', met gemeentenaam in kolom '* NomCPAS (nl)'. 





## Op welke tijdsperiode hebben deze data betrekking? Bv. "kalenderjaar 2023" of een uittreksel op “2022-21-31”. Hoe kan dit worden afgeleid uit de ruwe dataset?

Alle data werden verzameld als uittreksel in de maand januari van het jaartal in kolom 'Année'. De kolom 'PaiCPASBénAnMois (cd)' bevat het jaar met maand aangeplakt, de maand is steeds '01' .



## Welke parameterdimensies zijn aanwezig in de ruwe dataset, uitgedrukt in de parametercodes aangeleverd door de data-architect en -engineers. Welke subdimensies bevatten de parameters? Waar kunnen deze parameters worden gevonden in de aangeleverde data? 

parameter
geslacht
leeftijdsklasse
huishoudtype



## Waar staan de cijfers/measures in de aangeleverde data en welke mogelijke waarden kunnen deze aannemen? Wat is de betekenis van al deze waarden?

in kolommen ll en (eq. ll)

De waarden zijn aantallen:
- ofwel positieve gehele getallen vanaf 0, op te nemen in OBS_VALUE
- zowel lege (geen terugbetalingsformulier gekregen) als 0 (na regularisatie van een terugbetalingsformulier) betekent dat er geen personen zijn die (equivalent) leefloon ontvangen
- ontbrekende combinaties van de dimensies betekenen dat er geen leefloners zijn dus waarde 0




## Wat voor type measures zijn dit? Zijn het normale waarden? Of heeft de leverancier ze geschat of geïmputeerd of …?

Alle waarden zijn normale waarden (code ‘A’)



## Zijn er data in dit bestand die niet gepubliceerd mogen worden (de echte waarde werd aangeleverd door leverancier maar we mogen die niet publiceren)? Zo ja, hoe wordt dit aangegeven? Welke methode werd gebruikt voor de versluiering?

De leverancier legt geen restricties op (code ‘F’). We doen eigen SDC.










# Stap 3: aggregatie


## Moeten totaalcijfers voor hogere geografische niveaus zoals het Vlaamse Gewest en/of het Brusselse Gewest worden berekend? Zo ja, hoe gebeurt dit?

Ja, cijfer voor de drie gewesten en voor Blegië


## Moet er worden geaggregeerd over een tijdsdimensie zoals de berekening van jaarcijfers uit maandcijfers? Zo ja, hoe gebeurt dit?

Neen




## Moet er worden geaggregeerd over andere parameterdimensies? Zo ja, hoe gebeurt dit?

Leeftijd wordt in bredere categoriën samengevat namelijk: ???

ja, over geslacht, leeftijdscategoriën, huishoudtype, parameter (leefloon versus equivalent leefloon)



## Hoe moeten de geaggregeerde cijfers berekend worden?

Som



## Wat moet er gebeuren indien er versluierde of gemaskeerde waarden aanwezig zijn in de dataset?

alle waarden zijn geobserveerd.












# Stap 4: statistical disclosure controle




## Bevatten de parameters gevoelige informatie? 

De parameters bevatten gevoelige informatie. kennis over leefloon kan leiden tot discriminatie van personen. 




## Wat voor type cijfers worden er aangeleverd? 

De aangeleverde cijfers zijn aantallen.




## Bij frequenties, wat is de meest fijnmazige toegelaten totale groepsgrootte in publicaties waartegen de cijfers worden afgezet?




## Welke versluieringsregels moeten worden gevolgd? Bij twijfel, raadpleeg de expert statistical disclosure control.

Wt publiceert ABB? 














# Werkpunten






