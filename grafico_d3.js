// define a margens e outras constantes

const mar = {
    t: 20,
    r: 20,
    b: 20,
    l: 35
};  

let texto = 'Clique para expandir';
  
// captura as dimensoes do svg -- para tentar torná-la 
  
  let svg = d3.select("svg.d3-chart");
  const w = +svg.style("width").replace("px", "");
  const h = +svg.style("height").replace("px", "");
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
           
    svg.append("rect")
      .attr('x', caixa.l)
      .attr('y', caixa.t)
      .attr('height', caixa.b - caixa.t)
      .attr('width', caixa.r - caixa.l)
      .attr('class', 'destaque')
      .attr('id', 'destaque-1')
      .append('title')
      .text(texto);  
    
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
    
    let update_selection = svg
      .selectAll('circle')
      .data(data, d => d.Empresa) // define key function
    
    update_selection
      .enter()
      .append('circle')
        .attr('cy', d => scale_y(d.result))
        .attr('cx', d => scale_x(d.PL))
        .attr('r', 4)
        .attr('opacity', 0.5)
        .attr('fill', d => scale_color(d.Dependencia));
    
// interactions
    
    // clique no rect
    
    d3.select('rect.destaque')
          .on('click', function() {
      
      let rect_atual = d3.select(this);
      console.log(rect_atual.attr('id'));
      
      
      
      data = data.filter(d => (d.PL >= limites1.PL[0] &
                                    d.PL <= limites1.PL[1]) &
                                    (d.result >= limites1.Lucro[0] &
                                     d.result <= limites1.Lucro[1]))
      //console.log('dentro do rect: ', data);
      
      // atualiza quantidade de empresas
      let qde_empresas = data.length;
      caixa_quantidade.text(qde_empresas);
      
      //atualiza escala
      scale_x.domain(d3.extent(data, d=>d.PL))
      scale_y.domain(d3.extent(data, d=>d.result))
      
      //atualiza dados
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
        .attr('cx', d => scale_x(d.PL))
      
      //oculta retângulo
      d3.select('rect.destaque')
        .attr('opacity', 1)
        .transition()
        .duration(1500)
        .attr('opacity', 0);
      
      //mostra novo retângulo

       const caixa2 = {
          t: scale_y(limites2.Lucro[1]),
          r: scale_x(limites2.PL[1]),
          b: scale_y(limites2.Lucro[0]),
          l: scale_x(limites2.PL[0])
        };

      d3.select('#destaque-2')
        .attr('x', caixa2.l)
        .attr('y', caixa2.t)
        .attr('height', caixa2.b - caixa2.t)
        .attr('width', caixa2.r - caixa2.l)
        .transition()
        .delay(500)
        .duration(1000)
        .attr('opacity', 1)       
   
      
      
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

    })

    // clique no rect2
    
    d3.select('rect#destaque-2')
          .on('click', function() {
      
      data = data.filter(d => (d.PL >= limites2.PL[0] &
                                    d.PL <= limites2.PL[1]) &
                                    (d.result >= limites2.Lucro[0] &
                                     d.result <= limites2.Lucro[1]))
      //console.log('dentro do rect: ', data);
      
      // atualiza quantidade de empresas
      let qde_empresas = data.length;
      caixa_quantidade.text(qde_empresas);
      
      //atualiza escala
      scale_x.domain(d3.extent(data, d=>d.PL))
      scale_y.domain(d3.extent(data, d=>d.result))
      
      //atualiza dados
      let pontos = svg.selectAll('circle')
        .data(data, d=>d.Empresa);
      
      //exit selection, remove pontos
      pontos
        .exit()
        .transition()
        .duration(1000)
        .attr('opacity', 0)
        .remove();
      
      //update selection, reposiciona
      pontos
                   .transition()
        .delay(500)
                  .duration(1000)
        .attr('cy', d => scale_y(d.result))
        .attr('cx', d => scale_x(d.PL))
      
      //oculta retângulo
      d3.select('#destaque-2')
        .attr('opacity', 1)
        .transition()
        .duration(1500)
        .attr('opacity', 0);   
      
      // mostra novo retângulo
      const caixa3 = {
          t: scale_y(limites3.Lucro[1]),
          r: scale_x(limites3.PL[1]),
          b: scale_y(limites3.Lucro[0]),
          l: scale_x(limites3.PL[0])
        };

      d3.select('#destaque-3')
        .attr('x', caixa3.l)
        .attr('y', caixa3.t)
        .attr('height', caixa3.b - caixa3.t)
        .attr('width', caixa3.r - caixa3.l)
        .transition()
        .delay(500)
        .duration(1000)
        .attr('opacity', 1)  
      
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

    })
    
    // clique no rect3
    
    d3.select('rect#destaque-3')
          .on('click', function() {
      
      data = data.filter(d => (d.PL >= limites3.PL[0] &
                               d.PL <= limites3.PL[1]) &
                              (d.result >= limites3.Lucro[0] &
                               d.result <= limites3.Lucro[1]))
      //console.log('dentro do rect: ', data);
      
      // atualiza quantidade de empresas
      let qde_empresas = data.length;
      caixa_quantidade.text(qde_empresas);
      
      //atualiza escala
      scale_x.domain(d3.extent(data, d=>d.PL))
      scale_y.domain(d3.extent(data, d=>d.result))
      
      //atualiza dados
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
        .attr('cx', d => scale_x(d.PL))
      
      //oculta retângulo
      d3.select('rect#destaque-3')
        .attr('opacity', 1)
        .transition()
        .duration(1500)
        .attr('opacity', 0);
         

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
      
      //atualiza dados
      let pontos = svg.selectAll('circle')
        .data(data, d=>d.Empresa);
      
      d3.select('#destaque-3')
        .attr('opacity', 0);
      
      d3.select('#destaque-1')
        .attr('opacity', 0)
           .transition()
                  .duration(1000)
        .attr('opacity', 1);
      
      pontos
                   .transition()
                  .duration(1000)
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
         .duration(1000)
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
