# Load packages
library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinyWidgets)
library(ggplot2)
library(Hmisc)
library(reshape2)


# Load File
# Import dataset ----
data_table <- t(read.table("inputdata.tsv", header=TRUE, check.names=FALSE, row.names=1, sep="\t"))
colnames(data_table)<-gsub("[a-zA-Z]","",colnames(data_table))

height <- 800

## User Interface
ui <- dashboardPage(
	dashboardHeader(
		title="Shiny NMR"
	),
	dashboardSidebar(
		h5(strong("Samples List :")),
		fluidRow(
			column(5,
				style = "margin-left: -15px;",
				actionButton(
					inputId = "check_all",
					label = "Check all"
				)
			),
			column(5,
				actionButton(
					inputId = "uncheck",
					label = "Uncheck all"
				)
			)
		),
		uiOutput("sample_list"),
		hr(),
		fluidRow(
            actionButton(
                inputId = "export_png",
                label = "PNG",
                icon = icon("export", lib = "glyphicon")
            ),
	        bsPopover(
				id = "export_png",
				title = "",
				content = "Export the graph in PNG file.",
				placement = "bottom",
				trigger = "hover",
		  		options = NULL
		  	)
		)
	),
	dashboardBody(
		includeCSS("styles.css"),
	    uiOutput("hover_info",
		   	style = "position: absolute;"
	    ),
		plotOutput("graph",
			height = height,
			dblclick = "dblclick",
			brush = brushOpts(
			   	id = "brush",
			   	resetOnNew = TRUE
			),
		    hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")
		)
	)
)

## Server
server <- function(input, output, session){

	## Initialize variables
	melted_table <- reactiveVal(0)
	samples <- reactiveVal(0)
	palette <- reactiveVal(rainbow(length(rownames(data_table))))
	ranges <- reactiveValues(x = NULL, y = NULL)


	## Dynamic User Interface
	# Samples List
	output$sample_list <- renderUI({
		tagList(
			wellPanel(
				style = paste0("overflow-y:scroll; background-color: transparent; border-color: transparent; max-height: ",as.character(as.numeric(height)-200),"px"),
		    	fluidRow(
					bsCollapse(
						open=h4(paste0(capitalize("Samples"))),
			    		bsCollapsePanel(
							title = h4(paste0(capitalize("Samples"))),
			            	coloredCheckboxGroupInput(
								inputId = "select_samples",
								label = NULL,
								choices = rownames(data_table),
								selected = rownames(data_table),
								colors = palette()
							)
			            )
			        )
		    	)
			)
		)
	})
	# Hover Panel
	output$hover_info <- renderUI({

		hover <- input$plot_hover

	    point <- nearPoints(melted_table(), hover, xvar = "time", yvar = "intensity", threshold = 10, maxpoints = 1)
	    if (nrow(point) == 0) return(NULL)

	    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
	    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    	left_px <- left_pct * (hover$range$right - hover$range$left) - (hover$range$left * 4)
	    top_px <- top_pct * (hover$range$bottom - hover$range$top) - (hover$range$top * 2)

		style <- paste0("position:absolute; width:auto; height:auto; z-index:1000; background-color: rgba(245, 245, 245, 0.85); ","left:", left_px, "px; top:", top_px, "px;")

        wellPanel(
	    	style = style,
		    p(HTML(paste0(
		    	"<b>Sample: </b>", point$sample, "<br/>",
		        "<b>Retention time: </b>", point$time, "<br/>",
		        "<b>Intensity: </b>", round(point$intensity,4), "<br/>"
		    )))
        )
	})


	## Update Events
	# Update Samples List (Un/Check All)
	observeEvent(input$check_all, {
		updateCheckboxGroupInput(
			session = session, 
			inputId = "select_samples",
			selected = rownames(data_table),
		)
	})
	observeEvent(input$uncheck, {
		updateCheckboxGroupInput(
			session = session,
			inputId = "select_samples",
			selected = character(0)
		)
	})


	## Graph Event
	# Zoom
	observeEvent(input$dblclick, {
		brush <- input$brush
		if (!is.null(brush)) {
			ranges$x <- c(brush$xmin, brush$xmax)
			ranges$y <- c(brush$ymin, brush$ymax)
		} else {
			ranges$x <- NULL
			ranges$y <- NULL
		}
	})	

	## Export Event
	# Export PNG image in history
	observeEvent(input$export_png, {
		png(filename="plot.png", width=2000, height = 1000)
		plot(spectra() + guides(col=guide_legend(title="Samples")))
		dev.off()
		gx_put("plot.png")
	})


	## Plot
	# Build Dynamic Plot
	spectra <- reactive({
		melted_table <- melt(data_table, varnames=c("sample","time"), value.name="intensity")
		table <- subset(melted_table, melted_table[["sample"]] %in% input$select_samples)
		melted_table(table)

		palette <- palette()
		names(palette) <- sort(levels(melted_table[["sample"]]))
		palette(palette)

		spectra <- ggplot(data=table, aes(x=time, y=intensity, col=sample)) + geom_line() + scale_x_continuous(trans="reverse") + xlab("Time") + ylab("Intensity") + ggtitle("NMR Spectra") + theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5)) + guides(col=FALSE) + scale_colour_manual(name = unique(table[["sample"]]), values = palette) + coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
	})
	# Render Plot	
	output$graph <- renderPlot({
		spectra <- spectra()
		return(spectra)
	})


	## Functions
	# Checkbox Group Input
	coloredCheckboxGroupInput <- function(inputId, label, choices, selected, colors){
	  	div(
	  		id=inputId,
	  		class="form-group shiny-input-checkboxgroup shiny-input-container shiny-bound-input",
	    	HTML(paste0('<label class="control-label" for="',inputId,'">',label,'</label>')),
		    div(
		    	style="background-color:transparent;",
		    	class="shiny-options-group",
		    	HTML(paste0(
		    		'<div class="checkbox">',
		            	'<label>',
		                    '<input type="checkbox" name="', inputId, '" value="', choices, '"', ifelse(choices %in% selected, 'checked="checked"', ''), '/>',
		                    '<span ', ifelse(choices %in% selected, paste0('style="font-size: 16px; max-width: 100%; word-wrap: break-word; color:', colors[choices],'"'),'max-width: 100%; word-wrap: break-word; style="font-size: 16px;"'), '>',choices,'</span>',
	                    '</label>',
	                '</div>', collapse = " "
	            ))
		    )
	    )
	}	
}

shinyApp(ui, server)