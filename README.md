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

## Sådan får du mappen ned på din egen computer (kun første gang)

Åbn Terminal (Mac) eller RStudios Terminal-fane, og skriv:

```
git clone git@github.com:CStylerMM/StudygroupYAYAYA.git
```

Tryk Enter. Nu har du en mappe på din computer med alle filerne fra dette repo.

Åbn den derefter som et projekt i RStudio: File → New Project → Existing Directory, og vælg den nye `StudygroupYAYAYA`-mappe.

## Vi har nu en fælles mappe på vores computere

Når du har klonet repoet, ligger der nu en mappe på din egen computer der hedder `StudygroupYAYAYA`. Den ser sådan ud i Finder:

![Vores fælles mappe](README%20billeder/1.se%20vores%20f%C3%A6llesmappe.png)

Det er den samme mappe som ligger på GitHub, bare på din egen maskine. Alt hvad du ser her, kan du åbne, læse og redigere, ligesom du plejer med almindelige filer.

## Vi kan tilføje mapper og filer herinde

Du kan roligt oprette nye mapper og filer direkte i denne mappe, ligesom du ville i enhver anden mappe på din computer, f.eks. med højreklik → "Ny mappe", eller ved at gemme et nyt script fra RStudio direkte ind i mappen. De bliver ikke automatisk synlige for andre, det kræver et git push (se nedenfor).

![Du kan tilføje i mappen, waow](README%20billeder/2.du%20kan%20tilf%C3%B8je%20i%20mappen%20waow.png)

## Sådan tilgår og redigerer du et script

1. Åbn dit RStudio-projekt for StudygroupYAYAYA.
2. Find filen i "Files"-fanen nede til højre, og klik på den for at åbne den.
3. Rediger som du normalt ville i R, gem filen med Cmd+S.

## Sådan uploader du dine ændringer (så andre kan se dem)

Hvis vi vil have vores ændringer til at gå igennem til resten af gruppen, skal vi ind i RStudio, vælge "Terminal"-fanen (ligger som en fane ved siden af "Console"), og følge disse trin.

Skriv disse tre kommandoer, én for én, og tryk Enter efter hver:

```
git add .
```
Dette markerer alle dine ændringer til at blive gemt.

![Kør git add](README%20billeder/3.%20k%C3%B8r%20gitt%20add.png)

```
git commit -m "kort besked om hvad du har ændret"
```
Dette gemmer ændringen med en besked, f.eks. `"tilføjet script til OLA2"`. Skift beskeden ud hver gang, så den passer til hvad du faktisk har gjort.

![Kør næste del](README%20billeder/4.%20k%C3%B8r%20n%C3%A6ste%20del.png)

```
git push
```
Dette sender dine ændringer op på GitHub, så resten af gruppen kan se dem. Når du trykker Enter her, pusher den dine filer direkte over i vores fælles git-mappe på GitHub. Så snart det er kørt igennem uden fejl, kan resten af gruppen se dine ændringer.

![Push det](README%20billeder/5.%20push%20det.png)

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
