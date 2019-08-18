// define a margens e outras constantes

const mar = {
    t: 20,
    r: 20,
    b: 20,
    l: 35
};  

let texto = 'Clique para expandir';
let svg = d3.select("svg.d3-chart");

// captura as dimensoes do svg -- para tentar torná-la 
let w = document.body.clientWidth;
let h = document.body.clientHeight;

w = w > 680 ? 680 : w;
h = h > 420 ? 420 : h;

console.log(w,h);
  

  
// read data

d3.dsv(";", "dados_d3.csv", function(d){
    return {
      Estado: d.Estado,
      Empresa: d.emp,
      Dependencia: d.dep,
      result: +d.resultado,
      reg: d.REGIAO,
      PL: +d.PL.replace(",", ".")
    }
  }).then(function(data) {
    
    //console.table(data[1]);
    //console.log(data);
    
    const dados_originais = data;
    
    
   
// escalas
    
    let scale_x = d3.scaleLinear()
                     .domain(d3.extent(data, d=>d.PL))
                     .range([mar.l,w-mar.r]);
    let scale_y = d3.scaleLinear()
                     .domain(d3.extent(data, d=>d.result))
                     .range([h-mar.b,mar.t]);            
    let scale_color = d3.scaleOrdinal()
                          .domain(["Dependente", "Não Dependente", "Não Informado"])
                          .range(["#f2ac29", "#718c35", "#5c4b51"])
    // console.log(scale_y.domain(), scale_y.range(), scale_y(0));
    
    
// áreas de destaque
    
    const limites1 = {
      PL:    [0   , 3e9],
      Lucro: [-4e8, 5e8]
    };
    
    const limites2 = {
      PL:    [0   , 380e6],
      Lucro: [-50e6, 45e6]
    };
    
    const limites3 = {
      PL:    [0   , 40e6],
      Lucro: [-5e6, 5e6]
    };
    
    const caixa = {
      t: scale_y(5e8),
      r: scale_x(3e9),
      b: scale_y(-4e8),
      l: scale_x(0)
    };
      
    // rect destaque-1, já aparece desde o começo
    svg.append("rect")
      .attr('x', caixa.l)
      .attr('y', caixa.t)
      .attr('height', caixa.b - caixa.t)
      .attr('width', caixa.r - caixa.l)
      .attr('class', 'destaque')
      .attr('id', 'destaque-1')
      .append('title')
      .text(texto);  
    
    // placeholders para os próximos rects
    svg.append("rect")
        .attr('x', 0)
        .attr('y', 0)
        .attr('height', 0)
        .attr('width', 0)
        .attr('opacity', 0)
        .attr('class', 'destaque')
        .attr('id', 'destaque-2')
        .append('title')
        .text(texto);  
    
    svg.append("rect")
        .attr('x', 0)
        .attr('y', 0)
        .attr('height', 0)
        .attr('width', 0)
        .attr('opacity', 0)
        .attr('class', 'destaque')
        .attr('id', 'destaque-3')
        .append('title')
        .text(texto);        
    
// objeto que vai indexar novos e próximos limites, além 
// dos ids dos retângulos a entrar e sair com base nos cliques 
// em cada uma das areas de destaque

indice_destaque = {
  'destaque-1': {
    'novos_limites' : limites1,
    'proximos_limites' : limites2,
    'proxima_area_a_mostrar' : 'destaque-2',
    'area_a_ocultar' : 'destaque-1'
  },

  'destaque-2': {
    'novos_limites' : limites2,
    'proximos_limites' : limites3,
    'proxima_area_a_mostrar' : 'destaque-3',
    'area_a_ocultar' : 'destaque-2'
  },

  'destaque-3': {
    'novos_limites' : limites3,
    'proximos_limites' : '',
    'proxima_area_a_mostrar' : '',
    'area_a_ocultar' : 'destaque-3'
  },
}

// formatação valores
    
    let localeBrasil = {
        "decimal": ",",
        "thousands": ".",
        "grouping": [3],
        "currency": ["R$", ""]};
    
    let formataBR = d3.formatDefaultLocale(localeBrasil).format(",.0f");

    
// eixos
    
    let eixo_x = d3.axisBottom()
                   .scale(scale_x)
                   .tickFormat(function(d) {return formataBR(d/1e6)});
    let eixo_y = d3.axisLeft()
                   .scale(scale_y)
                   .tickFormat(function(d) {return formataBR(d/1e6)});
        
    svg.append("g")    
      .attr("class", "axis x-axis")
      .attr("transform", "translate(0," + (scale_y(0)) + ")")
      .call(eixo_x); 
    
    svg.append("g")    
      .attr("class", "axis y-axis")
      .attr("transform", "translate(" + mar.l + ")")
      .call(eixo_y); 
   
/// quantidade de empresas
    
    let qde_empresas = data.length;
    //console.log(qde_empresas);
    let caixa_quantidade = d3.select('span.qde-emp')
    caixa_quantidade
      .text(qde_empresas);
    
    
// plot
    
    let pontos = svg
      .selectAll('circle')
      .data(data, d => d.Empresa) // define key function
    
    pontos
      .enter()
      .append('circle')
        .attr('cy', d => scale_y(d.result))
        .attr('cx', d => scale_x(d.PL))
        .attr('r', 4)
        .attr('opacity', 0.5)
        .attr('fill', d => scale_color(d.Dependencia));
    
//////////////////////////////////// 
///////////// interactions /////////
////////////////////////////////////
    
    //////////// cliques nos rects /////////////

    // função generalizada

    const mostra_destaque = function(rect_atual) {

      console.log('entrei, pelo menos? tô no ', rect_atual);

      // busca novos limites do objeto indexador
      const limites = indice_destaque[rect_atual].novos_limites;

      // filtra dados com base nesses novos limites
      data = data.filter(d => (d.PL >= limites.PL[0] &
                               d.PL <= limites.PL[1]) &
                              (d.result >= limites.Lucro[0] &
                               d.result <= limites.Lucro[1])
                          )
      // atualiza quantidade de empresas
      let qde_empresas = data.length;
      caixa_quantidade.text(qde_empresas);
      
      //atualiza escala
      scale_x.domain(d3.extent(data, d=>d.PL))
      scale_y.domain(d3.extent(data, d=>d.result))

      //atualiza dados / rebind
      let pontos = svg.selectAll('circle')
                      .data(data, d=>d.Empresa);
      
      //exit selection, remove pontos
      pontos
        .exit()
        .transition()
        .duration(500)
        .attr('opacity', 0)
        .remove();
      
      //update selection, reposiciona
      pontos
        .transition()
        .delay(500)
        .duration(1000)
        .attr('cy', d => scale_y(d.result))
        .attr('cx', d => scale_x(d.PL));

      //oculta retângulo

      const rect_ocultar = indice_destaque[rect_atual].area_a_ocultar;
      console.log('area a ocultar: ', rect_ocultar);

      d3.select('#'+rect_ocultar)
        .attr('opacity', 1)
        .transition()
        .duration(1500)
        .attr('opacity', 0);

      // mostra novo retângulo

      const rect_mostrar = indice_destaque[rect_atual].proxima_area_a_mostrar;
      const proximos_limites = indice_destaque[rect_atual].proximos_limites;
      console.log('proximo rect:', rect_mostrar, proximos_limites);

      if (rect_mostrar != '') {
        // define as dimensões do novo retângulo
        const proximo_rect = {
          t: scale_y(proximos_limites.Lucro[1]),
          r: scale_x(proximos_limites.PL[1]),
          b: scale_y(proximos_limites.Lucro[0]),
          l: scale_x(proximos_limites.PL[0])
        };

        // atualiza os atributos do placeholder
        d3.select('#'+rect_mostrar)
          .attr('x', proximo_rect.l)
          .attr('y', proximo_rect.t)
          .attr('height', proximo_rect.b - proximo_rect.t)
          .attr('width', proximo_rect.r - proximo_rect.l)
          .transition()
          .delay(500)
          .duration(1000)
          .attr('opacity', 1)  
      }

      //Update X axis
      svg.select(".x-axis")
        .transition()
        .delay(500)
        .duration(1000)
        .call(eixo_x);

      //Update Y axis
      svg.select(".y-axis")
        .transition()
        .delay(500)
        .duration(1000)
        .call(eixo_y);
      
      // fim função
    }

    ///// clique rect1
    
    d3.select('#destaque-1')
          .on('click', function(){
            const rect_atual = d3.select(this).attr('id');
            mostra_destaque(rect_atual);
          })

    //// clique no rect2
    
    d3.select('#destaque-2')
          .on('click', function(){
            const rect_atual = d3.select(this).attr('id');
            mostra_destaque(rect_atual);
          })
    
    //// clique no rect2
    
    d3.select('#destaque-3')
          .on('click', function(){
            const rect_atual = d3.select(this).attr('id');
            mostra_destaque(rect_atual);
          })   
    
    // clique no parágrafo para reiniciar
    
    d3.select('p.reinicia')
          .on('click', function() {
      
      data = dados_originais
      console.log('dentro do reinicia: ', data);
      
      // atualiza quantidade de empresas
      let qde_empresas = data.length;
      caixa_quantidade.text(qde_empresas);
      
      //atualiza escala
      scale_x.domain(d3.extent(data, d=>d.PL))
      scale_y.domain(d3.extent(data, d=>d.result))
      
      //atualiza dados / rebind
      let pontos = svg.selectAll('circle')
        .data(data, d=>d.Empresa);
      
      // oculta última área de destaque
      d3.select('#destaque-3')
        .attr('opacity', 0);
      
      // reexibe área de destaque inicial
      d3.select('#destaque-1')
        .attr('opacity', 0)
        .transition()
        .duration(1000)
        .attr('opacity', 1);
      
      pontos
        .transition()
        .duration(1500)
        .attr('cy', d => scale_y(d.result))
        .attr('cx', d => scale_x(d.PL));
      
      pontos
         .enter()
         .append('circle')
         .attr('cy', d => scale_y(d.result))
         .attr('cx', d => scale_x(d.PL))
         .attr('r', 0)
         .attr('opacity', 0.5)
         .attr('fill', d => scale_color(d.Dependencia))
         .transition()
         .delay(1000)
         .duration(500)
         .attr('r', 4);
      
      
      
      //Update X axis
      svg.select(".x-axis")
          .transition()
          .duration(1000)
            .call(eixo_x);
      
      //Update Y axis
      svg.select(".y-axis")
          .transition()
          .duration(1000)
          .call(eixo_y);
    })
    
  });
