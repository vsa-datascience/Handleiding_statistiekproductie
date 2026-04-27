---
output:
  word_document:
    reference_docx: "template.docx"
title: "Procedure aanlevering NKREDNEM (Aantal kredietnemers)"    
---



# Versies

| Versie | Auteur | Aanpassing                              |
| -----: | ------ | --------------------------------------- |
| 1      | Jorre  | Eerste versie versie in eerste template |
| 2      | Jorre  | Procedure voor SDC aangepast |








# Algemene informatie




## Het emailadres van VSA-medewerker die de data verzamelt en doorstuurt

annelies.jacques@vlaanderen.be



## Uitgebreide omschrijving van de inhoud van de data-aanlevering:

Aantal personen dat op 31 december van het jaar bij de Centrale voor Kredieten aan Particulieren van de Nationale Bank van België geregistreerd staat met minstens één wanbetaling voor een consumentenkrediet (verrichtingen op afbetaling en kredietopeningen) of hypothecair krediet, afgesloten door natuurlijke personen voor privédoeleinden. De cijfers worden opgesplitst naar 5 types krediet:

- Aantal kredietnemers met minstens één niet geregulariseerd achterstallig krediet
- Aantal kredietnemers met minstens één niet geregulariseerd achterstallig hypothecair krediet
- Aantal kredietnemers met minstens één niet geregulariseerd achterstallige lening op afbetaling
- Aantal kredietnemers met minstens één niet geregulariseerd achterstallige verkoop op afbetaling
- Aantal kredietnemers met minstens één niet geregulariseerd achterstallige kredietopening



## Oplijsting van de doelen van de data-aanlevering:

- Aanmaak van VOS 552: Betalingsachterstand en collectieve schuldenregeling
- Aanmaak van GSM-indicator AR_01: “Aantal personen dat op 31 december van het jaar bij de Centrale voor Kredieten aan Particulieren van de Nationale Bank van België geregistreerd staat met minstens één wanbetaling voor een consumentenkrediet (verrichtingen op afbetaling en kredietopeningen) of hypothecair krediet, afgesloten door natuurlijke personen voor privédoeleinden, ten opzichte van de bevolking van 18 jaar en ouder op 1 januari van jaar x+1.”.



## Verwachte dimensies en onderverdelingen in de data:

- Per kalenderjaar
- Per Vlaamse gemeente volgens NIS-code en voor de drie gewesten
- Per type achterstallig krediet (zie uitgebreide omschrijving)



## Noodzakelijke aanleverfrequentie van de data: 

jaarlijks



## Afspraak over de structuur van de bestandsnaam van de aangeleverde data: 

De data worden aangeleverd als xlsx bestand met naam “personen_met_betalingsachterstand_YYYY.xlsx” waarbij YYYY wordt vervangen door het jaar van de aanlevering.











# Stap 1: verzamelen van de ruwe data



## bron/producent van de cijfers: 

Nationale Bank van België (NBB), Centrale voor kredieten aan particulieren (CKP)



## Weblink naar de producent of de webpagina waar de data werden gedownload:

https://www.nbb.be/nl/kredietcentrales/centrale-voor-kredieten-aan-particulieren-ckp/alles-over-de-ckp/de-ckp-cijfers



## Beschrijving van het verzamelingsproces: 

De data werden gedownload van de website van de Nationale Bank: https://www.nbb.be/nl/kredietcentrales/centrale-voor-kredieten-aan-particulieren-ckp/alles-over-de-ckp/de-ckp-cijfers. Onderaan de pagina, onder de titel ‘Detailcijfers’ kunnen de cijfers worden gedownload in xlsx-formaat:

- Onder titel ‘Per jaar volgens postcode’ wordt per kalenderjaar een bestand voorzien:
  
  + In elk bestand zijn het ‘aantal kredietnemers met minstens één niet geregulariseerd achterstallig krediet’ te vinden in het tabblad ‘Algemeen overzicht’ in de kolom ‘Aantal kredietnemers met minstens één ... Achterstallig krediet’.
  + De aantallen kredietnemers met minstens één niet geregulariseerd achterstallig(e) hypothecair krediet, lening op afbetaling, verkoop op afbetaling, en kredietopening zijn te vinden in tabblad ‘Achterstallige kredietnemers’.
  + Al deze data wordt aangeboden per postcode.

- In het bestand achter de link ‘Totaal en regionaal’:
  + In tabbladen ‘BR-Algemeen overzicht’  en ‘VL-Algemeen overzicht’  voor Brussel en Vlaanderen respectievelijk zijn het ‘aantal kredietnemers met minstens één niet geregulariseerd achterstallig krediet’ te vinden in de kolom ‘Aantal kredietnemers met minstens één ... Achterstallig krediet’. Hierin worden telkens de cijfers genomen in rij met ‘Maand==YYYY-12’ voor jaar YYYY.
  + In tabbladen ‘BR-Achterstallige kredietnemers’ en ’VL-Achterstallige kredietnemers’ voor Brussel en Vlaanderen respectievelijk aantallen kredietnemers met minstens één niet geregulariseerd achterstallig(e) hypothecair krediet (kolom ‘Onroerend hypothecair krediet’), lening op afbetaling, verkoop op afbetaling, en kredietopening. Hierin worden telkens de cijfers genomen in rij met ‘Maand==YYYY-12’ voor jaar YYYY.
  + Al deze data wordt aangeboden per NIS-code.

De data worden handmatig gebundeld in één xlsx-bestand door de analist. In het gebundeld bestand zitten de data van elk jaar in aparte tabbladen. Elk tabblad bevat een tabel met de postcodes in de rijen en de vijf types kredieten in de kolommen. De cellen bevatten het aantal personen met achterstallige betalingen. 

Het bestand bevat ook een apart tabblad met de totalen voor het Vlaamse en Brusselse gewest. De data in dit tabblad zijn op een andere manier gestructureerd. De rijen worden opgedeeld tussen het Vlaams en Brussels gewest en per jaar. In de kolommen staan opnieuw de vijf types kredieten. De cellen bevatten opnieuw de aantallen personen met achterstallige betalingen.





## Beschrijving van hoe de dataleverancier/bron de cijfers verzamelde: 

De cijfers geven het aantal kredietnemers dat op 31 december van het jaar bij de Centrale voor Kredieten aan Particulieren van de Nationale Bank van België geregistreerd staat met minstens 1 achterstallig krediet. Het gaat om achterstallen op consumentenkredieten (leningen op afbetaling, verkopen of afbetaling en kredietopeningen) en (on)roerende hypothecaire kredieten, afgesloten door natuurlijke personen voor privédoeleinden.
De criteria die aanleiding geven tot de registratie van achterstallen op kredietovereenkomsten zijn de volgende:

- voor de verkopen op afbetaling, de leningen op afbetaling en de financieringshuren en de hypothecaire kredieten onder die vorm:
  + wanneer 3 termijnbedragen op hun vervaldag niet of onvolledig zijn betaald, of
  + wanneer een vervallen termijnbedrag gedurende 3 maanden niet of onvolledig is betaald, of
  + wanneer de nog te vervallen termijnbedragen onmiddellijk opeisbaar geworden zijn.
- voor de kredietopeningen en de hypothecaire kredieten onder die vorm:
  + wanneer een bedrag aan kapitaal en/of totale kosten komt te vervallen overeenkomstig de voorwaarden van de kredietovereenkomst en dit niet of onvolledig werd terugbetaald binnen een termijn van 3 maanden, of
  + wanneer het kapitaal volledig opeisbaar is geworden en het verschuldigde bedrag niet of onvolledig werd terugbetaald, of
  + wanneer het totaal terug te betalen bedrag niet werd terugbetaald binnen de maand na het verstrijken van de nulstellingstermijn.

Annelies vraagt nog na bij de Nationale Bank hoe die registers precies worden aangevuld en up-to-date gehouden.



























# Stap 2: transformatie naar gekuiste data en validatie



## Over welke geografische eenheden is er informatie aanwezig in de ruwe dataset. Waar is de informatie over de geografische eenheden te vinden in de dataset. Als de dataset geen expliciete NIS-codes, postcodes of andere geografische codes bevat in overeenstemming met de codelijsten, hoe moeten die dan worden afgeleid? 

In het ruwe bestand hebben we informatie over postcodes, maar ook totalen voor het Vlaams en Brussels Gewest. De postcodes staan in de kolom 'postcode' in de verschillende tabbladen. Voor de cijfers van het Vlaams gewest in tabblad ‘vlaams Gewest’ moet NIScode 02000 worden toegekend. Voor het Brussels Gewest NIS-code 04000 volgens de NIS5 indeling van 01/01/2019.




## Op welke tijdsperiode hebben deze data betrekking? Bv. "kalenderjaar 2023" of een uittreksel op “2022-21-31”. Hoe kan dit worden afgeleid uit de ruwe dataset?

Alle data werden verzameld als uittreksel op 31 december van het jaartal dat gebruikt wordt als naam van de tabbladen of in kolom C staat in tabblad ‘Gewesten’ .



## Welke parameterdimensies zijn aanwezig in de ruwe dataset, uitgedrukt in de parametercodes aangeleverd door de data-architect en -engineers. Welke subdimensies bevatten de parameters? Waar kunnen deze parameters worden gevonden in de aangeleverde data? 

Het gaat over volgende groepen kredietnemers (KREDTYPE):

- ‘achtrst_totaal’ in kolom 'Aantal kredietnemers met minstens één niet geregulariseerd achterstallig krediet'
- 'achtrst_hypkred' in kolom 'Aantal kredietnemers met minstens één niet geregulariseerd achterstallig hypothecair krediet'
- 'achtrst_lening' in kolom 'Aantal kredietnemers met minstens één niet geregulariseerd achterstallige lening op afbetaling'
- 'achtrst_verkoop' in kolom 'Aantal kredietnemers met minstens één niet geregulariseerd achterstallige verkoop op afbetaling'
- 'achtrst_kropen' in kolom 'Aantal kredietnemers met minstens één niet geregulariseerd achterstallige kredietopening'



## Waar staan de cijfers/measures in de aangeleverde data en welke mogelijke waarden kunnen deze aannemen? Wat is de betekenis van al deze waarden?

De waarden zijn aantallen:
- ofwel positieve gehele getallen vanaf 0, op te nemen in OBS_VALUE
- ofwel waarde '<3' om kleine aantallen aan te duiden die werden versluierd door de Nationale Bank. Op te nemen in OBS_VALUE_C en missende waarde in OBS_VALUE. Soms komt ook waarde ‘< 3’ voor (met een spatie in het midden), deze moet omgezet worden naar ‘<3’.




## Wat voor type measures zijn dit? Zijn het normale waarden? Of heeft de leverancier ze geschat of geïmputeerd of …?

Alle waarden zijn normale waarden (code ‘A’)



## Zijn er data in dit bestand die niet gepubliceerd mogen worden (de echte waarde werd aangeleverd door leverancier maar we mogen die niet publiceren)? Zo ja, hoe wordt dit aangegeven? Welke methode werd gebruikt voor de versluiering?

Alle waarden mogen gepubliceerd worden (code ‘F’). Sommige waarden werden reeds versluierd door de dataleverancier en staan in de dataset aangeduid met waarde '<3', deze mogen ook gewoon gepubliceerd worden. De dataset bevat geen expliciete waarden die we niet mogen publiceren. 











# Stap 3: aggregatie


## Moeten totaalcijfers voor hogere geografische niveaus zoals het Vlaamse Gewest en/of het Brusselse Gewest worden berekend? Zo ja, hoe gebeurt dit?

De data-engineers gebruiken een vaste standaard mapping om postcodes te vertalen naar NIS-codes voor de berekening van de cijfers op gemeente- en provincieniveau. Voor de totaalcijfers voor het Vlaamse en Brusselse Gewest worden de cijfers gebruikt zoals aangeleverd.



## Moet er worden geaggregeerd over een tijdsdimensie zoals de berekening van jaarcijfers uit maandcijfers? Zo ja, hoe gebeurt dit?
Er dient geen aggregatie te gebeuren over een tijdsdimensie.



## Moet er worden geaggregeerd over andere parameterdimensies? Zo ja, hoe gebeurt dit?

Er dient geen aggregatie te gebeuren over een parameterdimensie. (In de vorige jaren diende dit wel te gebeuren omdat er slechts één indicator werd aangeleverd aan ABB, maar de teller van deze indicator werd berekend als een som van twee parameters. De afspraken met ABB zijn nu echter aangepast en alle vijf parameters worden vertaald naar vijf aparte indicatoren.)



## Hoe moeten de geaggregeerde cijfers berekend worden?

Geaggregeerde cijfers worden berekend door alle cijfers op het lager niveau op te tellen.



## Wat moet er gebeuren indien er versluierde of gemaskeerde waarden aanwezig zijn in de dataset?

Gemaskeerde waarden (’<3’) worden vervangen door het cijfer ‘0’ om de som op hoger niveau te kunnen berekenen. Als een totaalcijfer werd berekend op zo’n gemaskeerde waarde wordt er voor het cijfer ‘>=’ geplakt om duidelijk te maken dat het echte cijfer groter is dan het bewaarde cijfer in de database.












# Stap 4: statistical disclosure controle




## Bevatten de parameters gevoelige informatie? 

De parameters bevatten gevoelige informatie. Kennis over achterstallige kredieten kan leiden tot discriminatie van personen. 




## Wat voor type cijfers worden er aangeleverd? 

De aangeleverde cijfers zijn aantallen.




## Bij frequenties, wat is de meest fijnmazige toegelaten totale groepsgrootte in publicaties waartegen de cijfers worden afgezet?

De aantallen worden vergeleken met het totaal aantal kredietnemers, niet de totale bevolkingsgrootte. Het totaal aantal kredietnemers wordt immers openlijk gepubliceerd op de website van de Nationale Bank, en is een fijnmaziger cijfer dan de bevolkingsgrootte. 



## Welke versluieringsregels moeten worden gevolgd? Bij twijfel, raadpleeg de expert statistical disclosure control.

We passen de volgende versluieringsregels toe:

+ Versluiering als het aantal personen met achterstallige krediet kleiner is dan 3.
+ Versluiering als het totaal aantal kredietnemers min het aantal personen met achterstallige krediet kleiner is dan 3.
+ Wanneer een geaggregeerd aantal op hoger niveau wordt berekend op basis van één versluierd cijfer samen met andere onversluierde cijfers, wordt het tweede laagste cijfer in deze reeks ook versluierd.















# Werkpunten


- Procedure moet worden herzien zodat er geen manuele manipulatie van de data meer is voor de data wordt doorgestuurd naar de data-engineers.
- Er moet een rekenregel worden afgetoetst voor de aggregatie van cijfers bij versluierde waarden (‘<3’) 
- De vraag moet worden gesteld aan de Nationale Bank of we koppeling kunnen maken met hun database in plaats van de data te downloaden vanop de website
- De vraag moet worden gesteld aan de Nationale Bank of we onversluierde gegevens kunnen krijgen  met 
  + Ofwel een vlag om versluierde waarden aan te geven
  + Ofwel duidelijke rekenregels zodat we zelf versluiering kunnen toepassen op dezelfde manier als de Nationale Bank.



