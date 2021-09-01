const vis_mapa = {

    config : {

        urls_dados : {

            lista_setores:   "./dados/lista-setores.csv",
            mapa_setores: "./dados/mapa-topo.json"

        },

        dimensoes : {

            w   : null,
            h   : null,
            dim : null

        },

        ref : {

            svg  : "svg.vis-mapa",
            cont : "figure.vis-mapa"
        },

        cores : {

            padrao : "#EFEFEF",
            divisa : "ghostwhite"

        }

        
    },

    state : {

        setor : null

    },

    data : {

        lista_setores   : null,
        mapa_setores    : null,
        tabela_cores    : {}
    
    },

    utils : {

        remove_acentos : function(str) {

            str = str.split(" ")[0];

            return str.replace(/[^a-zA-Z ]/g, "");

        },

        monta_escala_cores : function() {

            vis_mapa.data.lista_setores.forEach(linha => {

                let key = vis_mapa.utils.remove_acentos(linha.setor);

                vis_mapa.data.tabela_cores[key] = linha.cores;

            });

        },

        resize : function() {

            let w = window.innerWidth;
            let h = window.innerHeight;

            let dim = w > 500 ? 500 : w;

            d3.select(vis_mapa.config.ref.svg)
              .style("height", dim)
              .style("width", dim);

            vis_mapa.config.dimensoes.w = w;
              //+cont.style("width").slice(0,-2);
  
            vis_mapa.config.dimensoes.h = h;
            vis_mapa.config.dimensoes.dim = dim - 10;

        }

    },

    fs : {

        start : function() {

            Promise.all(

            [
                d3.csv(vis_mapa.config.urls_dados["lista_setores"]),
                d3.json(vis_mapa.config.urls_dados["mapa_setores"])
            ]
            
            ).then(function(files) {
          
              vis_mapa.data.lista_setores   = files[0];
              vis_mapa.data.mapa_setores = files[1];

              vis_mapa.fs.init();

            })
        },

        init : function() {

            //console.table(vis_mapa.data.lista_setores);
            //console.table(vis_mapa.data.mapa_setores);

            vis_mapa.fs.popula_lista(vis_mapa.data.lista_setores);
            vis_mapa.fs.ajusta_altura_box();
            vis_mapa.fs.controla_seletor();
            vis_mapa.utils.monta_escala_cores();
            vis_mapa.utils.resize();
            vis_mapa.fs.constroi_mapa();
        
        },

        mostra_box_setor : function(setor) {

            //console.log("Me chamaram para ativar o setor ", setor);

            d3.selectAll(".box-definicao").classed("ativo", false);

            d3.select("#box" + setor).classed("ativo", true);

        },

        controla_seletor : function() {

            let seletor = d3.select("#seletor-setores");

            seletor.on("change", function() {

                let opcao_escolhida = seletor.property("value");

                opcao_escolhida = vis_mapa.utils.remove_acentos(opcao_escolhida);
                vis_mapa.state.setor = opcao_escolhida;

                //console.log("Opa, mudança! Vou ativar o setor ", opcao_escolhida);

                vis_mapa.fs.mostra_box_setor(opcao_escolhida);

                vis_mapa.fs.pinta_mapa(opcao_escolhida);

            });
            
        },

        popula_lista : function(dados_lista) {

            let seletor = d3.select("#seletor-setores");

            seletor
              .selectAll("option")
              .data(dados_lista)
              .join("option")
              .attr("value", d => d.setor)
              .text(d => d.setor);

            let boxes = d3.select("div.boxes-definicao");

            boxes = boxes
              .selectAll("div.box-definicao")
              .data(dados_lista)
              .join("div")
              .classed("box-definicao", true)
              .attr("id", d => ("box" + vis_mapa.utils.remove_acentos(d.setor)));

            boxes
              .append("h2")
              .text(d => d.setor)
              .style("color", d => d.cores);

            boxes
              .append("p")
              .text(d => d.def)
              .style("background-color", d => d.cores)
              .style("color", (d,i) => i > 14 ? "rgb(59,55,52)" : "ghostwhite");

        },

        ajusta_altura_box : function() {

            let altura_max = 0;

            let boxes = d3.select("div.boxes-definicao").selectAll("div.box-definicao");
            boxes.each(function() {

                let no = d3.select(this).node();

                let altura = no.getBoundingClientRect().height;

                altura_max = Math.max(altura_max, altura);

            });

            d3.selectAll(".box-definicao").style("height", altura_max + "px");
            d3.select(vis_mapa.config.ref.cont).style("margin-top", (altura_max + 20) + "px");

        },

        constroi_mapa : function() {

            let dim = vis_mapa.config.dimensoes.dim;

            let topodata = vis_mapa.data.mapa_setores;

            let feats = 
              topojson.feature(
                topodata, 
                topodata.objects.estados)
              .features;

            let geodata = {

                type:"FeatureCollection",
                "features": feats

            };

            console.log(geodata);


            let projecao = d3
              .geoConicEqualArea()
              .parallels([-33.8, 5.3])
              .rotate([40, 0])
              .fitSize([dim, dim], geodata)
            ;

            let svg = d3.select("svg.vis-mapa");


            svg.append("g")
              .selectAll("path")
              .data(feats)
              .join("path")
              .attr("fill", vis_mapa.config.cores.padrao)
              .attr("stroke", vis_mapa.config.cores.divisa)
              .attr("d", d3.geoPath().projection(projecao))
            ;

        },

        pinta_mapa : function(setor) {

            let svg = d3.select("svg.vis-mapa");

            svg
              .select("g")
              .selectAll("path")
              .transition()
              .duration(400)
              .attr(
                  "fill", 
                  d => 
                  d.properties[setor] == 1 ?
                  vis_mapa.data.tabela_cores[setor] :
                  vis_mapa.config.cores.padrao
                )
            ;

        }

    }

}

vis_mapa.fs.start();


