# StudygroupYAYAYA

Her hygger vi med DATAanalyse.

Dette er stedet hvor vi deler vores R-scripts, noter, data og bøger til Data Analyse-faget. Denne guide forklarer hvordan du henter filer ned, redigerer dem, og lægger dine ændringer op igen, sådan at alle andre kan se dem.

## Hvad ligger hvor

- **Bøger** – vores 3 bøger som pdf/html
- **Casper's DATA** – Caspers datafiler, excel, csv og lign.
- **Casper's r.scripts** – næsten alle scripts Casper har lavet
- **CheatSheet** – cheat sheets til R
- **FÆLLES DATA** – her uploader vi fælles data, excel-ark, csv-filer osv.
- **Fælles** – åben mappe til noter og lignende
- **Johans kurser** – Johans kurser
- **OLA1** – overblik over OLA1 og rettelser
- **OLA2** – overblik over OLA2

## Sådan tilgår og redigerer du et script

1. Åbn dit RStudio-projekt for StudygroupYAYAYA.
2. Find filen i "Files"-fanen nede til højre, og klik på den for at åbne den.
3. Rediger som du normalt ville i R, gem filen med Cmd+S.

## Sådan uploader du dine ændringer (så andre kan se dem)

Åbn Terminal-fanen i RStudio (ligger som en fane ved siden af "Console"). Skriv disse tre kommandoer, én for én, og tryk Enter efter hver:

```
git add .
```
Dette markerer alle dine ændringer til at blive gemt.

```
git commit -m "kort besked om hvad du har ændret"
```
Dette gemmer ændringen med en besked, f.eks. `"tilføjet script til OLA2"`. Skift beskeden ud hver gang, så den passer til hvad du faktisk har gjort.

```
git push
```
Dette sender dine ændringer op på GitHub, så resten af gruppen kan se dem.

## Sådan henter du andres nye filer ned

Inden du selv begynder at arbejde, er det en god idé at hente det andre har lagt op. Skriv i Terminal:

```
git pull
```

## Hvis du er i tvivl om noget er gemt

Skriv:

```
git status
```

Den fortæller dig altid hvad der er sket, og hvad du eventuelt mangler at gøre.

## Kort opsummeret

1. `git pull` (hent nyeste)
2. Rediger dine filer
3. `git add .`
4. `git commit -m "besked"`
5. `git push`
