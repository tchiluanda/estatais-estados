const cards = {

    data : {

        _raw : null,
        _filtered : null,       
        _summarised : null,
        _current_setores : null,
        _current_emp : null,
        _estados : null,

        load : () => {

            d3.csv('./dados/dados_cards.csv').then(data => {

                cards.ctrl.after_data_is_loaded(data);

            })


        },

        filter : (estado_selecionado) => {

            cards.data._filtered = estado_selecionado != 'Todos' ? 
              cards.data._raw.filter(d => d.Nome_estado == estado_selecionado) :
              cards.data._raw
            ;

        },

        get_current_setores : () => {

            cards.data._current_setores = cards.utils.unique(cards.data._filtered, "setor");

        },

        get_estados : () => {

            cards.data._estados = cards.utils.unique(cards.data._raw, "Nome_estado");


        },

        make_summary : () => {

            const data = cards.data._filtered;

            cards.data._summarised = cards.utils.group_and_count(data, "setor");

        },

        update : (estado_selecionado) => {

            cards.data.filter(estado_selecionado);
            cards.data.get_current_setores();

        }

    },

    utils : {

        group_and_count : function(data, categoria) {

            const resultado = []; 

            const categorias_unicas = cards.utils.unique(data, categoria);

            for (cat of categorias_unicas) {
              const sub_data = data
                              .filter(d => d[categoria] === cat);
                              
              resultado.push({"categoria" : cat,
                              "subtotal"  : sub_data.length});   
            }

            return resultado;

        },

        unique : function(data, coluna) {

            return data
              .map(d => d[coluna])
              .filter((v, i, a) => a.indexOf(v) === i);

        },

        formata_valor : (valor) => {

            const localeBrasil = {
                "decimal": ",",
                "thousands": ".",
                "grouping": [3],
                "currency": ["R$", ""]
            };

            const formataBR   = d3.formatDefaultLocale(localeBrasil).format(",.0f");

            const multiplos = [1, 1e3, 1e6, 1e9, 1e12];
            const sufixo    = ["", "mil", "mi", "bi", "tri"];

            const obj_mult = multiplos.map((d,i) => ({
            valor: d,
            sufixo: sufixo[i]
            }));
            //console.log("objeto multiplos", obj_mult)

            const valor_formatado = function(x) {
                for (mult of obj_mult) {
                    const val = x/mult.valor;
                    if (val < 1000) return formataBR(val) + " " + mult.sufixo;
                }
            }

            return valor_formatado(valor);

        }

    },

    vis : {

        ref : 'div.vis-cards',

        prepare_groups : () => {

            const data = cards.data._current_setores;

            const cont = d3.select(cards.vis.ref);

            cont
              .selectAll('div.container-setor')
              .data(data)
              .join('div')
              .attr('data-container-setor', d => d)
              .classed('container-setor', true);

        },

        adds_companies : () => {

            const data = cards.data._current_setores;

            const cont = d3.select(cards.vis.ref);

            const cont_setores = cont.selectAll('div.container-setor');

            cont_setores.each(function(setor, i) {

                //console.log(d3.select(this), setor);

                const data = cards.data._filtered.filter(d => d.setor == setor);

                d3.select(this)
                  .selectAll('div.empresa')
                  .data(data, d => d[""]) // essa coluna "" contém o index que veio do R.
                  .join('div')
                  //.attr('data-emp', d => d.emp)
                  .classed('empresa', true)
                  .classed('dependente', d => d.dep == "Dependente")
                  .classed('indicios-dependencia', d => d.tipo_indicio != "NA")
                  .attr('title', d => d.emp);

            })

        },

        update : () => {

            cards.vis.prepare_groups();
            cards.vis.adds_companies();


        },

        monitora_clicks_hover : () => {

            const container = document.querySelector(cards.vis.ref);

            //container.addEventListener('hover', cards.card.show_card);
            container.addEventListener('click', cards.card.show_card);

        }



    },

    seletor : {

        ref : '#seletor-estados',

        populate : () => {

            const sel = document.querySelector(cards.seletor.ref);

            const estados = ['Todos', ...cards.data._estados];
            
            estados.forEach( (estado) => {

                const new_option = document.createElement('option');

                new_option.value = estado;
                new_option.text = estado;

                sel.appendChild(new_option);

            });

        },

        update : (e) => {

            const option = e.target.value;

            //console.log(option);

            //esconde o card
            cards.card.toggle(false);

            cards.data.update(option);
            cards.vis.update();

        },

        monitora : () => {

            const sel = document.querySelector(cards.seletor.ref);

            sel.addEventListener('change', cards.seletor.update);


        }

    },

    card : {

        ref : '.card-container',

        toggle : (toggle) => {

            const card = document.querySelector(cards.card.ref);
            const msg = document.querySelector('.mensagem');

            // toggle é um boolean, se vier true, o card.hidden vai ser falso, pq quero mostrar o card.

            card.hidden = !toggle;
            msg.hidden = toggle;

        },

        populate_texts : (empresa) => {

            const cont = document.querySelector(cards.card.ref);

            const data = cards.data._filtered.filter(d => d.emp == empresa)[0];
            cards.data._current_emp = data; // salva numa propriedade para evitar fazer isso de novo.

            cont.dataset.dependencia = data['dep'];

            const fields = cont.querySelectorAll('[data-cardinfo]');

            fields.forEach(field => {

                const coluna = field.dataset.cardinfo;
                //console.log(coluna, data[coluna]);


                field.innerHTML = data[coluna];

            })

            // link

            const link_carta = data['link'];
            const link = document.querySelector('.card-container [href]');

            if (link_carta != 'NA') {

                link.hidden = false;
                link.href = data['link'];

            } else {

                link.hidden = true;

            }

            // tipo indicio

            const campo_indicio = cont.querySelector('[data-tipo-indicio]');
            campo_indicio.dataset.tipoIndicio = data['tipo_indicio'];
            
        },

        show_card : (e) => {

            if (e.target.classList.contains('empresa') ) {

                // tô usando vanilla, então pego o nome da empresa do atributo title ~data attribute~.
                // podia ter usado o evento do D3, pq aí pegaria o próprio datum que está bound ao elemento.

                const emp = e.target.title; //e.target.dataset.emp;

                // marca o selecionado, mas antes remove de todos os outros;
                const empresas = document.querySelectorAll('div.empresa');
                empresas.forEach(empresa => { empresa.classList.remove('selected') });
                e.target.classList.add('selected');

                // popula e mostra o card
                cards.card.populate_texts(emp);
                cards.card.toggle(true);
                cards.card.mini_vis.update_rects_and_texts();

            }

        },

        mini_vis : {

            params : {

                h_barra: 20,
                pad : 30,

                colunas_valores : ['capital', 'desp_investimento', 'lucros']

            },

            scales : {

                w : d3.scaleLinear(),

                set : () => {

                    // // maxs

                    // const colunas = cards.card.mini_vis.params.colunas_valores;

                    // const maxs = colunas.map(coluna => d3.max(cards.data._raw, d => +d[coluna]));

                    // const max = Math.max(...maxs);

                    // // domain

                    // cards.card.mini_vis.scales.w.domain([0, max]);

                    // range

                    const cont = document.querySelector(cards.card.ref);
                    
                    const width = 280;//+(window.getComputedStyle(cont).width.slice(0,-2));
                    cards.card.mini_vis.scales.w.range([0, width]);

                }

            },

            update_rects_and_texts : () => {

                const data = cards.data._current_emp;
                const colunas = cards.card.mini_vis.params.colunas_valores;
                const w = cards.card.mini_vis.scales.w;

                // calcula maximo dos valores e atualiza o domínio
                const valores = colunas.map( coluna => Math.abs(+data[coluna]) );
                const current_max = Math.max(...valores);
                const flag_zerada = current_max == 0;

                w.domain([0, current_max]);
                // 

                const rects = document.querySelectorAll('.mini-vis-rect');

                rects.forEach(rect => {

                    const coluna = rect.dataset.minivisColuna;

                    let valor = data[coluna];

                    let sinal = "";

                    const campo_valor = document.querySelector([`[data-cardinfo-value="${coluna}"]`]);

                    if (valor.slice(0,1) == "-") {

                        valor = valor.slice(1);
                        rect.classList.add("negativo");
                        campo_valor.classList.add("negativo");

                        sinal = "-";

                    } else {

                        rect.classList.remove("negativo");
                        campo_valor.classList.remove("negativo");

                    }

                    rect.style.width = flag_zerada ? 0 : w(+valor) + 'px';

                    // valores
                    campo_valor.innerHTML = "R$ " + sinal + cards.utils.formata_valor(valor);

                })

            }


        }

    },

    ctrl : {

        init : () => {

            cards.data.load();
            // this function will then call after_data_is_loaded() below

            


        },

        after_data_is_loaded : (data) => {

            cards.data._raw = data;

            //console.log(data);

            cards.data.get_estados();
            cards.seletor.populate();
            cards.seletor.monitora();

            cards.card.mini_vis.scales.set();

            // when a filter is selected, this will have to change
            cards.data.update(estado_selecionado = 'Todos');

            cards.vis.update();
            cards.vis.monitora_clicks_hover();

        }

    }


}

cards.ctrl.init();