# Load required libraries
library(shiny)
library(stringr)
library(ggplot2)
library(ggstats)
library(reshape2)
library(formattable)

# Importa os dados
respostas = read.table("respostas_organizadas_2025.txt", header = TRUE, sep = "\t")

# Arruma a resposta "Muito Ruim" para "Muito ruim"
respostas$resposta[respostas$resposta == "Muito Ruim"] = "Muito ruim"

# Transforma a coluna 'resposta' em um fator com a ordem correta dos níveis
respostas$resposta = factor(respostas$resposta,
                            levels = c("Muito ruim", "Ruim", 
                                       "Regular", 
                                       "Bom", "Muito bom", 
                                       "Não sei / Não conheço", 
                                       "Inexistente / Não se aplica", 
                                       "1", "2", "3", "4", "5", 
                                       "6", "7", "8", "9", "10"))

respostas$dimensao_numero = as.numeric(gsub("Dimensão ", "",
                                            word(respostas$dimensao, 1, sep = "\\:")))

# Converte o numero da "dimensao" Satisfacao em zero
respostas$dimensao_numero[which(is.na(respostas$dimensao_numero))] = 0


# Define the UI for the app
ui <- fluidPage(
  titlePanel("Respostas ao Questionário da CPA do IFFar | Ano-base 2025"),
  
  # Layout com barra lateral
  sidebarLayout(
    sidebarPanel(
      selectInput("campus", "Selecione o campus:", choices = c("Todos", sort(unique(respostas$campus)))),
      selectInput("segmento", "Selecione o segmento:", choices = c("Docente", "Técnico Administrativo em Educação", "Sociedade Civil", "Discente")),
      uiOutput("curso_ui"),
      selectInput("dimensao", "Selecione a dimensão:", choices = sort(unique(respostas$dimensao))[c(1, 3:10, 2)])
    ),
    
    mainPanel(
      plotOutput("ggplot"),
      formattableOutput("tabela")
    )
  )
)

# Define the server logic
server <- function(input, output, session) {
  
  # Função para filtrar os dados com base nos inputs
  filtered_data <- reactive({
    # Verifique se "Todos" foi selecionado para 'campus' ou 'curso'
    campus_filter <- if (input$campus == "Todos") TRUE else (respostas$campus == input$campus)
    curso_filter <- if (input$curso == "Todos") TRUE else (respostas$curso == input$curso)
    
    # Filtrando os dados com base no segmento e nas seleções de campus e curso
    subset(respostas,
           campus_filter & 
             segmento == input$segmento & 
             curso_filter & 
             dimensao == input$dimensao)
  })
  
  # Modificando a seleção de 'curso' baseada no 'segmento' e 'campus'
  observe({
    # Se o segmento for "Docente", "Técnico Administrativo em Educação" ou "Sociedade Civil", não há filtro de curso
    if (input$segmento %in% c("Docente", "Técnico Administrativo em Educação", "Sociedade Civil")) {
      updateSelectInput(session, "curso", choices = c("Todos", sort(unique(respostas$curso))), selected = "Todos")
      output$curso_ui <- renderUI({
        selectInput("curso", "Selecione o curso:", choices = c("Todos", sort(unique(respostas$curso))))
      })
    } else {
      updateSelectInput(session, "curso", choices = c("Todos", sort(unique(respostas$curso))), selected = "Todos")
      output$curso_ui <- renderUI({
        selectInput("curso", "Selecione o curso:", choices = c("Todos", sort(unique(respostas$curso))))
      })
    }
  })
  
  # Calcular a tabela e o gráfico sempre que os dados forem filtrados
  output$ggplot <- renderPlot({
    respostas_subset <- filtered_data()
    
    # Verifique se os dados filtrados estão vazios
    if (nrow(respostas_subset) == 0) {
      return(NULL)  # Se não houver dados, não gera o gráfico
    }
    
    # Calcula a quantidade de respostas por questão
    pt = tapply(respostas_subset$resposta,
                list(respostas_subset$questao_curta, respostas_subset$resposta),
                length)
    
    # Converte a tabela em data frame
    pt = as.data.frame(pt)
    pt[is.na(pt)] = 0
    
    # Remove colunas da escala de satisfação, mantendo todas as colunas relevantes
    pt = pt[, 1:7]
    
    # Atribui um peso para cada resposta
    peso = as.vector((pt$`Muito bom` + pt$Bom) / rowSums(pt) * 100)
    pt = pt[order(-peso), ]
    peso = as.vector((pt$`Muito bom` + pt$Bom) / rowSums(pt) * 100)
    
    # Categorias e cores
    categoria = rep("DESENVOLVER", nrow(pt))
    categoria[peso >= 75] = "MANTER"
    categoria[peso < 25] = "INTERVIR"
    categoria[peso < 50 & peso >= 25] = "CORRIGIR"
    
    cor_categoria = rep("#92c5de", nrow(pt))
    cor_categoria[peso >= 75] = "#0571b0"
    cor_categoria[peso < 25] = "#ca0020"
    cor_categoria[peso < 50 & peso >= 25] = "#f4a582"
    
    # Reordena a tabela para o formato longo
    pt$questao_curta = rownames(pt)
    pt_melt = melt(pt, id = "questao_curta")
    pt_melt$questao_curta = factor(pt_melt$questao_curta, levels = unique(pt_melt$questao_curta))
    
    # Labels
    labels = categoria
    label_positions <- data.frame(label = labels, x = rep(1, length(labels)), y = 1:length(labels))
    
    # Gráfico
    ggplot(pt_melt,
           aes(y = questao_curta, x = value, fill = variable)) +
      labs(x = NULL, y = NULL, fill = NULL,
           title = unique(respostas_subset$eixo),
           subtitle = unique(respostas_subset$dimensao),
           caption = paste(unique(respostas_subset$segmento),
                           unique(respostas_subset$campus),
                           sep = " | ")) +
      geom_bar(stat = "identity", 
               position = position_likert(exclude_fill_values = c("Regular", "Não sei / Não conheço", "Inexistente / Não se aplica"))) +
      geom_vline(xintercept = 0, linewidth = 2/3) +
      scale_x_continuous(label = label_percent_abs(), breaks = seq(-0.75, 0.75, 0.25)) +
      scale_fill_brewer(type = "div", palette = "RdBu") +
      theme_minimal(base_size = 16) +
      theme(legend.position = "bottom",
            panel.grid.minor = element_blank(),
            axis.text.y = element_text(size = 12)) +
      geom_label(data = label_positions, aes(x = x, y = y, label = label), hjust = 1,
                 color = "white", fill = cor_categoria, size = 4,
                 fontface = "bold", label.padding = unit(0.35, "lines"))
  })
  
  # Exibir a tabela formatada
  output$tabela <- renderFormattable({
    respostas_subset <- filtered_data()
    
    # Verifique se os dados filtrados estão vazios
    if (nrow(respostas_subset) == 0) {
      return(NULL)  # Se não houver dados, não gera a tabela
    }
    
    pt = tapply(respostas_subset$resposta,
                list(respostas_subset$questao_curta, respostas_subset$resposta),
                length)
    
    pt = as.data.frame(pt)
    pt[is.na(pt)] = 0
    
    peso = as.vector((pt$`Muito bom` + pt$Bom) / rowSums(pt) * 100)
    pt = pt[order(peso), 1:7]
    
    tabela = round(pt / rowSums(pt) * 100, 1)
    tabela$Respondentes = rowSums(pt)
    
    formattable(tabela)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
