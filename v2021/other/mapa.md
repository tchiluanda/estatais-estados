## O Mapa

Processamento do mapa, para fazer a visualização dos setores, seguindo um tutorial do Mike Bostock.

https://github.com/topojson/topojson-server/blob/master/README.md#topology

```bash
/Users/tchiluanda/node_modules/topojson-server/bin/geo2topo mapa-setores.geojson > mapa.json
```

com 4 digitos ficou 1.5Mb, com 2, 3.1Mb


### Mapas

https://medium.com/@mbostock/command-line-cartography-part-1-897aa8f8ca2c

https://brew.sh/

Baixei o mapa com geobr. Manipulei no R.

O mapa estava muito grande, aí simplifiquei com

```r
mapa <- st_simplify(mapa, dTolerance = .1)
```

Bem interessante brincar com esse dTolerance!

Aí salvei em SHP com

```r
st_write(mapa_qde_export, "./dados/mapa-setores/mapa-setores.shp")
```

Aí usei `shapefile` do node para converter.

```bash
npm install -g shapefile
shp2json mapa-setores.shp -o mapa-setores.json
```

We could now display this in a browser using D3, but first we should apply a geographic projection. By avoiding expensive trigonometric operations at runtime, the resulting GeoJSON renders much faster, especially on mobile devices. Pre-projecting also improves the efficacy of simplification, which we’ll cover in part 3. To install d3-geo-projection’s command-line interface:

```bash
npm install -g d3-geo-projection
```

Precisava descobrir os limites extremos do Brasil em termos de paralelos, aí usei isso aqui, no olho:

```r
ggplot(mapa) + 
  geom_sf() + 
  geom_hline(yintercept = 5.3, color = "red") + 
  geom_hline(yintercept = -33.8, color = "red")
```

O comando do geoproject então vai ficar

```bash
geoproject 'd3.geoConicEqualArea().parallels([-33.8, 5.3]).rotate([40, 0]).fitSize([960, 960], d)' < mapa-setores.json > mp-set-conic.json
```

This d3.geoConicEqualArea projection is California Albers, and as its name suggests, is appropriate for showing California. It’s also equal-area, which is strongly recommended for choropleth maps as the projection will not distort the data. If you’re not sure what projection to use, try d3-stateplane or search spatialreference.org.

The projection you specify to geoproject is an arbitrary JavaScript expression. That means that you can use projection.fitSize to fit the input geometry (represented by d) to the desired 960×960 bounding box!
To preview the projected geometry, use d3-geo-projection’s geo2svg:

```bash
geo2svg -w 960 -h 960 < mp-set-conic.json > mapa.svg
```


https://medium.com/@mbostock/command-line-cartography-part-2-c3a82c5c0f3

Often the data we find online isn’t precisely what we want. Perhaps we need to join multiple data sources, or to convert formats (say from fixed-width text to CSV), or even just to drop unneeded fields to produce smaller files that are faster to download.

You can always write scripts to transform data in arbitrary ways, but writing (and debugging) scripts can become tedious as transformations become more complex. What we want is an iterative, exploratory approach, where at every step we can see what the data looks like; by inspecting as we go, we can fix mistakes as we make them, before they get buried in complexity. And when we’re done, we want to capture our workflow as machine-readable documentation that can be easily reproduced.

The command line is great for this. UNIX has a well-established philosophy of small, decoupled tools that allow powerful manipulation of data. To leverage the power of the command line, we simply need to convert our data into a format that fits a UNIX convention: lines of text. And since JSON is already text, we just need each line of text be a valid JSON value.

Enter newline-delimited JSON (NDJSON), which is simply JSON values separated by newlines (\n). NDJSON combines the best of both worlds: the convenience of the command line for working with files, and the power of JavaScript and its myriad open-source modules. My ndjson-cli module provides tools for converting JSON to NDJSON, for manipulating NDJSON streams (filtering, mapping, joining) and more. To install:

```bash
npm install -g ndjson-cli
```

To convert a GeoJSON feature collection to a newline-delimited stream of GeoJSON features, use ndjson-split:

```
ndjson-split 'd.features' \
  < mp-set-conic.json \
  > mp-set-conic.ndjson
```

Even better, we can use d3-geo-projection to quickly generate an SVG choropleth from the command line! To do that, first install D3:

```bash
npm install -g d3
```

Next use ndjson-map, requiring D3 via -r d3, and defining a fill property using a sequential scale with the Viridis color scheme:

```bash
ndjson-map -r d3 '(d.properties.fill = d3.scaleOrdinal().domain([0, 1]).range(["#efefef", "#0D0887"])(d.properties.ABASTEC), d)' < mp-set-conic.ndjson > mp-set-conic-color.ndjson
```

To convert the newline-delimited GeoJSON to SVG using geo2svg:

```bash
geo2svg -n --stroke none -p 1 -w 960 -h 960 < mp-set-conic-color.ndjson > mp-set-conic.svg
```


The GeoJSON feature collection of census tracts we constructed previously was 13.6M. That’s fine for local viewing, but a bit large for the web! Fortunately there are ways to shrink geometry without apparent loss of detail. We can:

Simplify (e.g., remove coordinates per Visvalingham).
Quantize (e.g., remove digits, say 224.3021507494117 to 224.3).
Compress (e.g., remove redundant geometry).

These are possible with GeoJSON, but we can do even better if we switch to a JSON dialect designed for efficient transport: TopoJSON. TopoJSON files are often 80% smaller than GeoJSON files, even without simplification. How is it so concise?

First, TopoJSON represents lines and polygons as sequences of arcs rather than sequences of coordinates. Contiguous polygons (census tracts, counties, states, etc.) have shared borders whose coordinates are duplicated in GeoJSON. With hierarchical geometry, such as counties that compose into states, there’s even more duplication! By representing lines and polygons as sequences of arcs, repeating an arc does not require repeating coordinates. (For more, see How to Infer Topology.)


To get started, install the TopoJSON CLI:

```bash
npm install -g topojson
```

Use geo2topo to convert to TopoJSON, reducing its size to 8.1M:

```bash
geo2topo -n \
  estados=mapa-proj.ndjson \
  > mapa-proj-topo.json
```
  
The slightly peculiar syntax, tracts=…, allows you to specify multiple named GeoJSON inputs, resulting in a topology with multiple named objects (or “layers”). Arcs can be shared across all objects in a topology.

Now to toposimplify, further reducing to 3.1M:

```bash
toposimplify -p 1 -f \
  < mapa-proj-topo.json \
  > mapa-simples-topo.json
```

The -p 1 argument tells toposimplify to use a planar area threshold of one square pixel when implementing Visvalingham’s method; this is appropriate because we previously applied a conic equal-area projection. If simplifying before projecting, use -s and specify a minimum-area threshold in steradians instead. The -f says to remove small, detached rings—little islands, but not contiguous tracts—further reducing the output size.

Lastly to topoquantize and delta-encode, reducing to 1.6M:

```bash
topoquantize 1e5 \
  < mapa-simples-topo.json \
  > mapa-quant-topo.json.json
```

As you can see, this is visually identical to the original, yet a tenth the size! Gzip (performed automatically by most servers) further reduces the transfer size to a svelte 390K.
Now suppose we want to overlay county borders on our choropleth map of census tracts. Most readers probably aren’t familiar with the geography of census tracts, so county outlines provide a helpful cue. (If we were making a national choropleth, we might similarly want state borders.)
The Census Bureau also publishes county boundaries, but we don’t actually need them. TopoJSON has another powerful trick up its sleeve: since census tracts compose hierarchically into counties, we can derive county geometry using topomerge!



No final, ficou assim:

1. pega mapa pelo geobr
2. simplifica com st_simplify
3. junta os dados (gera o mapa_qde_export)
4. exporta em geojson com qde de dígitos limitada ou não
(nao) 5. projeta o mapa usando geoproject
6. converte para ndjson com `ndjson-split 'd.features' < mapa-setores.geojson > mapa-setores.ndjson`
7. converte para topojson com geo2topo `geo2topo -n estados=mapa-setores.ndjson > mapa-topo.json`
8. toposimplify `toposimplify -p 1 -f < mapa-topo.json > mapa-simples.json`
9. topoquantize `topoquantize 1e5 < mapa-simples.json > mapa.json`


In order to use fitSize/fitExtent we need to create a geojson object that contains all the individual features, a feature collection does this, the structure is as follows:

```js
featureCollection = {
  "type":"FeatureCollection",
  "features": [ ... ]
}
```
(https://stackoverflow.com/questions/56516358/d3-js-set-fitextent-or-fitsize-or-center-and-scale-programmatically-for-map-w)
