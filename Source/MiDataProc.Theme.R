library(shinydashboard)

# creating custom theme object
customTheme <- shinyDashboardThemeDIY(

  ### general
  appFontFamily = "Arial"
  ,appFontColor = "rgb(0,0,0)"
  ,primaryFontColor = "rgb(0,0,0)"
  ,infoFontColor = "rgb(0,0,0)"
  ,successFontColor = "rgb(0,0,0)"
  ,warningFontColor = "rgb(0,0,0)"
  ,dangerFontColor = "rgb(0,0,0)"
  ,bodyBackColor = "rgb(255,255,255)"

  ### header
  ,logoBackColor = "rgb(2,123,255)"

  ,headerButtonBackColor = "rgb(2,123,255)"
  ,headerButtonIconColor = "rgb(2,123,255)"
  ,headerButtonBackColorHover = "rgb(2,123,255)"
  ,headerButtonIconColorHover = "rgb(0,0,0)"

  ,headerBackColor = "rgb(2,123,255)"
  ,headerBoxShadowColor = "#aaaaaa"
  ,headerBoxShadowSize = "0px 0px 0px"

  ### sidebar
  ,sidebarBackColor = "rgb(24,31,41)" # 사이드바 뒤 배경 색깔
  ,sidebarPadding = 0

  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0

  ,sidebarShadowRadius = ""
  ,sidebarShadowColor = "0px 0px 0px"

  ,sidebarUserTextColor = "rgb(24,31,41)"

  ,sidebarSearchBackColor = "rgb(255, 255, 255)"
  ,sidebarSearchIconColor = "rgb(24,31,41)"
  ,sidebarSearchBorderColor = "rgb(24,31,41)"

  ,sidebarTabTextColor = "rgb(210,210,210)"
  ,sidebarTabTextSize = 14
  ,sidebarTabBorderStyle = "none"
  ,sidebarTabBorderColor = "none"
  ,sidebarTabBorderWidth = 0

  ,sidebarTabBackColorSelected = "rgb(45,52,63)"
  ,sidebarTabTextColorSelected = "rgb(252,255,255)"
  ,sidebarTabRadiusSelected = "0px"

  ,sidebarTabBackColorHover = "rgb(67,75,86)"
  ,sidebarTabTextColorHover = "rgb(252,255,255)"
  ,sidebarTabBorderStyleHover = "none"
  ,sidebarTabBorderColorHover = "none"
  ,sidebarTabBorderWidthHover = 0
  ,sidebarTabRadiusHover = "0px"

  ### boxes
  ,boxBackColor = "rgb(245,245,245)"
  ,boxBorderRadius = 3
  ,boxShadowSize = "0px 0px 0px"
  ,boxShadowColor = "rgba(0,0,0,0)"
  ,boxTitleSize = 16
  ,boxDefaultColor = "rgb(210,214,220)"
  ,boxPrimaryColor = "rgb(35, 49, 64)"
  ,boxInfoColor = "rgb(2,123,255)"
  ,boxSuccessColor = "rgb(112,173,71)"
  ,boxWarningColor = "rgb(244,156,104)"
  ,boxDangerColor = "rgb(255,88,55)"

  ,tabBoxTabColor = "rgb(255,255,255)"
  ,tabBoxTabTextSize = 14
  ,tabBoxTabTextColor = "rgb(0,0,0)"
  ,tabBoxTabTextColorSelected = "rgb(35, 49, 64)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgb(2,123,255)"
  ,tabBoxBorderRadius = 0

  ### inputs
  ,buttonBackColor = "rgb(245,245,245)"
  ,buttonTextColor = "rgb(0,0,0)"
  ,buttonBorderColor = "rgb(24,31,41)"
  ,buttonBorderRadius = 3

  ,buttonBackColorHover = "rgb(227,227,227)"
  ,buttonTextColorHover = "rgb(100,100,100)"
  ,buttonBorderColorHover = "rgb(200,200,200)"

  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(200,200,200)"
  ,textboxBorderRadius = 0
  ,textboxBackColorSelect = "rgb(245,245,245)"
  ,textboxBorderColorSelect = "rgb(200,200,200)"

  ### tables
  ,tableBackColor = "rgb(255, 255, 255)"
  ,tableBorderColor = "rgb(245, 245, 245)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1

)
