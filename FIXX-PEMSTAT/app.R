# ================================================================
#                 CLT APP - RETRO UI (SAME FEATURES)
# ================================================================

library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(readr)
library(readxl)
library(DT)

addResourcePath("img", "www")

# ======================= Helper: baca CSV / XLSX =======================
read_any <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "csv") {
    readr::read_csv(path, show_col_types = FALSE)
  } else if (ext %in% c("xlsx", "xls")) {
    readxl::read_excel(path)
  } else {
    stop("Format file tidak didukung. Gunakan .csv atau .xlsx/.xls")
  }
}

# ================================================================
#                           UI
# ================================================================
ui <- tagList(
  tags$head(
    tags$style(HTML("
      :root{
        --cream:#f8ede3;
        --beige:#dfd3c3;
        --tan:#d0b8a8;      /* coklat muda */
        --brown:#8d493a;
        --ink:#2f2525;
        --accent:#c66a52;

        --stroke:2px solid var(--ink);
        --shadow: 6px 6px 0 rgba(47,37,37,0.95);
        --shadow-soft: 10px 10px 0 rgba(47,37,37,0.18);
        --r-lg:18px;
        --r-md:14px;
      }

      html, body { margin:0; padding:0; background:var(--cream); color:var(--ink); }

      body:before{
        content:'';
        position:fixed; inset:0;
        background:
          repeating-linear-gradient(0deg, rgba(47,37,37,0.06) 0 2px, transparent 1px 56px),
          repeating-linear-gradient(90deg, rgba(47,37,37,0.06) 0 2px, transparent 1px 56px);
        pointer-events:none;
        z-index:0;
      }
      .container-fluid, .navbar, .navbar-default { position:relative; z-index:1; }

      .page-wrap { padding-left: 28px; padding-right: 28px; }
      @media (max-width: 768px) { .page-wrap { padding-left: 14px; padding-right: 14px; } }

      .navbar-default{
        background:var(--cream) !important;
        border:0 !important;
        border-bottom: var(--stroke) !important;
        box-shadow:none !important;
      }
      .navbar{ margin-bottom:0; }

      .navbar .container-fluid{
        padding-left:18px; padding-right:18px;
        display:flex; align-items:center;
      }
      .navbar-header{
        flex:0 0 26%;
        max-width:26%;
        display:flex; align-items:center; justify-content:flex-start;
      }
      .navbar-header .navbar-brand{
        float:none !important;
        font-weight:900;
        letter-spacing:0.5px;
        font-size:22px !important;
        color:var(--ink) !important;
        pointer-events:none;
      }
      .navbar-collapse{
        flex:0 0 74%;
        max-width:74%;
        display:flex; justify-content:flex-end; align-items:center;
      }
      .navbar-nav{
        width:100%;
        display:flex;
        justify-content:flex-end;
        gap:10px;
        margin:0;
      }
      .navbar-nav>li{ width:auto; }
      .navbar-default .navbar-nav>li>a{
        color:var(--ink) !important;
        font-weight:800;
        border-radius:12px;
        padding:10px 14px;
        border: var(--stroke);
        background: var(--cream);
        box-shadow: 4px 4px 0 rgba(47,37,37,0.35);
        transition: transform .15s ease, box-shadow .15s ease;
      }
      .navbar-default .navbar-nav>li>a:hover{
        transform: translate(-1px,-1px);
        box-shadow: 6px 6px 0 rgba(47,37,37,0.35);
        background: var(--tan);
      }
      .navbar-default .navbar-nav>.active>a,
      .navbar-default .navbar-nav>.active>a:focus,
      .navbar-default .navbar-nav>.active>a:hover{
        background: var(--accent) !important;
        color: var(--cream) !important;
        border: var(--stroke) !important;
        box-shadow: 6px 6px 0 rgba(47,37,37,0.6) !important;
      }

      @media (max-width:768px){
        .navbar-header, .navbar-collapse { max-width:100%; flex:0 0 100%; }
        .navbar-nav{ flex-wrap:wrap; justify-content:flex-start; }
      }

      .retro-card{
        background: var(--cream);
        border: var(--stroke);
        border-radius: var(--r-lg);
        box-shadow: var(--shadow);
      }
      .retro-card.soft{
        background: var(--beige);
        box-shadow: var(--shadow-soft);
      }
      .retro-pad{ padding:18px; }

      .retro-btn{
        border: var(--stroke) !important;
        border-radius: 12px !important;
        background: var(--accent) !important;
        color: var(--cream) !important;
        font-weight:900 !important;
        box-shadow: 6px 6px 0 rgba(47,37,37,0.6) !important;
        transition: transform .12s ease, box-shadow .12s ease;
      }
      .retro-btn:hover{
        transform: translate(-1px,-1px);
        box-shadow: 8px 8px 0 rgba(47,37,37,0.6) !important;
      }
      .retro-btn:active{
        transform: translate(2px,2px);
        box-shadow: 4px 4px 0 rgba(47,37,37,0.6) !important;
      }

      .retro-outline{
        border: var(--stroke);
        border-radius: var(--r-md);
        background: var(--cream);
        box-shadow: 4px 4px 0 rgba(47,37,37,0.28);
      }

      .panel-split{
        border: var(--stroke);
        border-radius: var(--r-lg);
        overflow:hidden;
        background: var(--cream);
        box-shadow: var(--shadow);
      }
      .panel-head{
        background: linear-gradient(135deg, rgba(141,73,58,0.16), rgba(208,184,168,0.5));
        border-bottom: var(--stroke);
        padding: 12px 14px;
      }
      .panel-head h3{ margin:0; font-weight:1000; letter-spacing:.2px; }
      .panel-body{ padding: 14px; background: var(--cream); }

      .well, .sidebarPanel{
        background: transparent !important;
        border:0 !important;
        box-shadow:none !important;
      }

      .form-control, .selectize-input{
        border: var(--stroke) !important;
        border-radius: 12px !important;
        box-shadow: 3px 3px 0 rgba(47,37,37,0.25) !important;
      }

      table.dataTable{
        border: var(--stroke) !important;
        border-radius: 12px;
        overflow:hidden;
      }

      .hero-wrap{
        display:grid;
        grid-template-columns: 1.2fr 0.8fr;
        gap:18px;
        align-items:stretch;
      }
      @media (max-width: 992px){
        .hero-wrap{ grid-template-columns: 1fr; }
      }
      .hero-left h1{
        font-size:44px;
        line-height:1.05;
        margin:0;
        font-weight:1000;
      }
      .hero-left p{
        margin-top:10px;
        font-size:15px;
        opacity:.92;
      }
      .hero-right{
        background: var(--accent);
        border: var(--stroke);
        border-radius: var(--r-lg);
        box-shadow: var(--shadow);
        display:flex;
        align-items:center;
        justify-content:center;
        position:relative;
        overflow:hidden;
      }
      .hero-right:before{
        content:'';
        position:absolute; inset:-40px;
        background:
          radial-gradient(circle at 20% 20%, rgba(248,237,227,0.35) 0 40%, transparent 42%),
          radial-gradient(circle at 80% 30%, rgba(248,237,227,0.22) 0 36%, transparent 38%),
          radial-gradient(circle at 55% 80%, rgba(248,237,227,0.22) 0 34%, transparent 36%);
        transform: rotate(8deg);
      }
      .hero-img{
        width:92%;
        max-width:520px;
        border: var(--stroke);
        border-radius: 22px;
        background: #ffd469;
        box-shadow: 10px 10px 0 rgba(47,37,37,0.35);
        padding: 10px;
        position:relative;
        z-index:1;
      }
      .hero-img img{
        width:100%;
        height:auto;
        border-radius: 16px;
        display:block;
      }

      .service-row{
        display:grid;
        grid-template-columns: 1fr 1fr 1fr;
        gap:12px;
      }
      @media (max-width: 992px){ .service-row{ grid-template-columns: 1fr; } }
      .service-item h4{ margin:0; font-weight:1000; }
      .service-item p{ margin:8px 0 0; font-size:13px; opacity:.9; }

      /* ✅ Kotak tujuan aplikasi (coklat muda) */
      .goal-box{
        background: var(--tan);
        border: var(--stroke);
        border-radius: var(--r-md);
        box-shadow: 6px 6px 0 rgba(47,37,37,0.35);
        padding: 14px 14px;
      }
      .goal-box h3{
        margin:0;
        font-weight:1000;
      }
      .goal-box ul{
        margin:10px 0 0;
        padding-left: 18px;
      }
      .goal-box li{
        margin:6px 0;
        font-weight:700;
      }

      .highlightBorder { border:2px solid #d9534f !important; box-shadow:0 8px 22px rgba(217,83,79,0.16) !important; animation:highlightPulse 1.6s ease-in-out; }
      @keyframes highlightPulse {
        0%{ box-shadow:0 0 0 0 rgba(217,83,79,0.0);}
        50%{ box-shadow:0 0 0 8px rgba(217,83,79,0.10);}
        100%{ box-shadow:0 0 0 0 rgba(217,83,79,0.0);}
      }
      .shake { animation:shakeAnim 0.8s; }
      @keyframes shakeAnim {
        0%{ transform:translateX(0);}
        20%{ transform:translateX(-6px);}
        40%{ transform:translateX(6px);}
        60%{ transform:translateX(-4px);}
        80%{ transform:translateX(4px);}
        100%{ transform:translateX(0);}
      }

      .symbol-table { width: 100% !important; table-layout: fixed; border-collapse: collapse; }
      .symbol-table th, .symbol-table td { padding: 10px 10px; vertical-align: top; word-wrap: break-word; overflow-wrap: anywhere; }
      .symbol-table th:nth-child(1), .symbol-table td:nth-child(1) { width: 20%; }
      .symbol-table th:nth-child(2), .symbol-table td:nth-child(2) { width: 28%; }
      .symbol-table th:nth-child(3), .symbol-table td:nth-child(3) { width: 52%; }

      /* ===================== TEAM CARDS: EQUAL SIZE + INTERACTIVE ========= */
      .team-card {
        min-height: 420px;
        display: flex;
        flex-direction: column;
        justify-content: flex-start;
        gap: 10px;
        transition: transform 0.28s cubic-bezier(0.2,0.8,0.2,1), box-shadow 0.28s;
        will-change: transform;
        position: relative;
        overflow: visible;
      }
      .team-card .team-img {
        width: 100%;
        height: 220px;
        object-fit: cover;
        border: var(--stroke);
        border-radius: 14px;
        box-shadow: 4px 4px 0 rgba(47,37,37,0.25);
        transition: transform 0.25s ease;
      }
      .team-card .team-body {
        display: flex;
        flex-direction: column;
        justify-content: space-between;
        flex: 1 1 auto;
      }
      .team-card .team-footer { margin-top: 8px; }
      .team-card:hover {
        transform: translateY(-12px);
        box-shadow: 12px 12px 0 rgba(47,37,37,0.45);
      }
      .team-card:hover .team-img { transform: translateY(-8px); }
      @media (max-width: 992px) {
        .team-card { min-height: 360px; }
        .team-card .team-img { height: 180px; }
      }
    ")),
    tags$script(HTML("
      Shiny.addCustomMessageHandler('highlight', function(msg) {
        try {
          var el = $('#' + msg.id);
          if (!el || el.length === 0) { el = $('[id=\"' + msg.id + '\"]'); }
          if (el && el.length > 0) {
            el.addClass('highlightBorder');
            if (msg.shake) el.addClass('shake');
            setTimeout(function() {
              el.removeClass('highlightBorder');
              if (msg.shake) el.removeClass('shake');
            }, msg.duration || 2000);
            try { el[0].scrollIntoView({behavior:'smooth', block:'center'}); } catch(e){}
          }
        } catch(e) { console.log(e); }
      });
    "))
  ),
  
  navbarPage(
    id    = "main_nav",
    title = div(span("CONVERRA")),
    theme = shinytheme("flatly"),
    fluid = TRUE,
    
    # ================== TAB 1: BERANDA ===================
    tabPanel(
      "Beranda", icon = icon("home"),
      fluidPage(
        div(class="page-wrap",
            br(),
            
            div(class="hero-wrap",
                # LEFT
                div(class="retro-card soft retro-pad hero-left",
                    tags$div(style="height:10px;"),
                    h1(HTML("<span style='color:var(--brown)'>Central Limit Theorem</span>")),
                    p("Upload data, lihat distribusi awal, jalankan simulasi CLT, dan dapatkan interval kepercayaan — semuanya dalam satu aplikasi."),
                    tags$div(style="height:12px;"),
                    
                    div(class="goal-box",
                        h3(HTML("<span style='color:var(--ink)'>Tujuan Aplikasi</span>"))
                    ),
                    tags$div(style="height:14px;"),
                    
                    div(class="service-row",
                        div(class="retro-outline retro-pad service-item",
                            h4(HTML("<span style='color:var(--brown)'>1.</span> Eksplorasi Data")),
                            p("Mengunggah data CSV dan melihat distribusi awal, statistik deskripti, serta karakteristik dasar data secara cepat")
                        ),
                        div(class="retro-outline retro-pad service-item",
                            h4(HTML("<span style='color:var(--brown)'>2.</span> Memahami CLT")),
                            p("Menyimulasikan rata rata sampel dari berbagai ukuran sehingga terlihat bagaimana distribusi rata rata sampel mendekati distribusi normal")
                        ),
                        div(class="retro-outline retro-pad service-item",
                            h4(HTML("<span style='color:var(--brown)'>3.</span> Inferensia Statistik")),
                            p("Menghitung dan menginterpretasikan interval kepercayaan untuk rata rata populasi sehingga dapat digunakan sebagai dasar pengambilan keputusan")
                        )
                    )
                ),
                
                # RIGHT
                div(class="hero-right",
                    div(class="hero-img",
                        tags$img(
                          src = "img/BERANDA_CLT.jpg",
                          onerror = "this.style.display='none'; this.parentElement.innerHTML='<div style=\"padding:14px; font-weight:900; text-align:center;\">Tambahkan gambar:<br><code>www/BERANDA_CLT.jpg</code><br><span style=\"font-weight:600; opacity:.85;\">(opsional)</span></div>';"
                        )
                    )
                )
            ),
            
            tags$div(style="height:14px;"),
            
            div(class="retro-card soft retro-pad",
                h3(style="margin-top:0; font-weight:1000;", "Langkah Cepat"),
                fluidRow(
                  column(3, actionButton("step1_box", "1. Upload Data", icon=icon("file-upload"), class="retro-btn", style="width:100%;")),
                  column(3, actionButton("step2_box", "2. Distribusi Awal", icon=icon("chart-area"), class="retro-btn", style="width:100%; background:var(--brown) !important;")),
                  column(3, actionButton("step3_box", "3. Jalankan Simulasi", icon=icon("play-circle"), class="retro-btn", style="width:100%;")),
                  column(3, actionButton("step4_box", "4. Interpretasi", icon=icon("lightbulb"), class="retro-btn", style="width:100%; background:var(--tan) !important; color:var(--ink) !important;"))
                )
            ),
            
            tags$div(style="height:16px;"),
            
            div(class="retro-card retro-pad",
                h3(style="margin-top:0; font-weight:1000;", "Video Tutorial (Placeholder)"),
                p(style="margin-bottom:10px;",
                  "Tempelkan link embed video (YouTube) ke atribut ", tags$code("src"), " pada iframe di bawah ini."
                ),
                div(
                  class="retro-outline",
                  style="position:relative; width:100%; aspect-ratio:16/9; border-radius:16px; overflow:hidden; background:#000;",
                  tags$iframe(
                    id="tutorial_video",
                    src="",
                    width="100%", height="100%",
                    style="position:absolute; inset:0; width:100%; height:100%; border:0;",
                    allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share",
                    allowfullscreen=NA
                  )
                )
            )
        )
      )
    ),
    
    # ================== TAB 2: TENTANG CLT ===================
    tabPanel(
      "Tentang CLT", icon = icon("info-circle"),
      fluidPage(
        div(class="page-wrap",
            br(),
            
            div(class="retro-card soft retro-pad",
                h2(style="margin:0; font-weight:1000;", "Ringkasan Materi Central Limit Theorem")
            ),
            
            tags$div(style="height:12px;"),
            
            div(class="retro-card retro-pad",
                h3(style="margin-top:0; font-weight:1000;", "Pengertian Central Limit Theorem"),
                p("Central Limit Theorem (CLT) adalah konsep penting dalam statistik yang menyatakan bahwa
                  rata-rata dari banyak sampel acak akan membentuk distribusi yang mendekati normal,
                  meskipun data asli tidak berdistribusi normal.
                  Karena itu, CLT memudahkan kita melakukan perhitungan seperti interval kepercayaan dan uji hipotesis
                  hanya dengan menggunakan sampel. CLT juga merupakan dasar utama inferensi statistik.")
            ),
            
            tags$div(style="height:12px;"),
            
            div(class="retro-card retro-pad",
                h4(style="margin-top:0; font-weight:1000;", "Inti CLT dalam 6 poin:"),
                tags$ol(
                  tags$li("Rata-rata sampel → semakin mendekati distribusi normal jika n besar."),
                  tags$li("Rata-rata distribusi rata-rata sampel ≈ mean populasi (μ)."),
                  tags$li("Variansi rata-rata sampel = σ²/n sehingga penyebaran makin kecil saat n membesar."),
                  tags$li("Sampel diambil secara acak dari populasi."),
                  tags$li("Pengamatan dalam sampel saling independen."),
                  tags$li("Ukuran sampel cukup besar (umumnya n ≥ 30) untuk bentuk distribusi populasi yang tidak terlalu ekstrem.")
                )
            ),
            
            tags$div(style="height:12px;"),
            
            div(class="retro-card retro-pad",
                h3(style="margin-top:0; font-weight:1000;", "Mengapa CLT sangat penting?"),
                p("Dengan CLT, kita dapat menggunakan pendekatan distribusi normal untuk:"),
                tags$ul(
                  tags$li("Menyusun dan menghitung interval kepercayaan untuk rata-rata populasi."),
                  tags$li("Melakukan uji hipotesis tentang rata-rata populasi."),
                  tags$li("Menyederhanakan perhitungan peluang yang melibatkan rata-rata sampel."),
                  tags$li("Membuat pendekatan normal untuk berbagai distribusi lain saat ukuran sampel cukup besar.")
                )
            ),
            
            tags$div(style="height:12px;"),
            
            div(class="retro-card retro-pad",
                h3(style="margin-top:0; font-weight:1000;", "Kegunaan CLT dalam kehidupan sehari-hari"),
                tags$ul(
                  tags$li(strong("Survei & polling: "), "mengestimasi rata-rata atau proporsi pendapat masyarakat."),
                  tags$li(strong("Kualitas produksi: "), "mengecek rata-rata karakteristik produk dari sampel."),
                  tags$li(strong("Bidang kesehatan: "), "mengestimasi rata-rata indikator kesehatan dari sebagian populasi."),
                  tags$li(strong("Bisnis & keuangan: "), "memperkirakan rata-rata penjualan/layanan dari data sampel."),
                  tags$li(strong("Penelitian ilmiah: "), "menyusun kesimpulan parameter populasi berdasarkan sampel.")
                )
            ),
            
            tags$div(style="height:12px;"),
            
            fluidRow(
              column(
                6,
                div(class="retro-card retro-pad",
                    h3(style="margin-top:0; font-weight:1000;", "Rumus CLT"),
                    div(style="text-align:center; margin-bottom:10px;",
                        tags$img(
                          src="img/RUMUS_CLT.png",
                          style="max-width:90%; height:auto; border-radius:14px; display:block; margin:0 auto; border:var(--stroke); box-shadow:4px 4px 0 rgba(47,37,37,0.25);"
                        )
                    ),
                    p(style="margin-bottom:0; opacity:0.9; font-size:13px;",
                      "Distribusi rata-rata sampel akan mendekati normal ketika ukuran sampel cukup besar."
                    )
                )
              ),
              column(
                6,
                div(class="retro-card retro-pad",
                    h3(style="margin-top:0; font-weight:1000;", "Keterangan Simbol pada Rumus CLT"),
                    tags$table(
                      class = "symbol-table",
                      style = "font-size:14px; width:100%; border:var(--stroke); border-radius:14px; overflow:hidden;",
                      tags$thead(
                        tags$tr(
                          tags$th("Simbol"),
                          tags$th("Arti"),
                          tags$th("Keterangan")
                        )
                      ),
                      tags$tbody(
                        tags$tr(tags$td(tags$b('\u0304X')), tags$td("Rata-rata sampel"), tags$td("Nilai rata-rata yang dihitung dari data sampel.")),
                        tags$tr(tags$td(tags$b('\u03BC')), tags$td("Rata-rata populasi"), tags$td("Nilai rata-rata sebenarnya dari seluruh populasi.")),
                        tags$tr(tags$td(tags$b('\u03C3')), tags$td("Simpangan baku populasi"), tags$td("Mengukur seberapa menyebar data populasi terhadap rata-ratanya.")),
                        tags$tr(tags$td(tags$b("n")), tags$td("Ukuran sampel"), tags$td("Banyaknya observasi dalam satu sampel.")),
                        tags$tr(tags$td(tags$b("Z")), tags$td("Z-score"), tags$td("Menunjukkan seberapa jauh nilai dari mean dalam satuan simpangan baku.")),
                        tags$tr(tags$td(tags$b("N(0,1)")), tags$td("Normal baku"), tags$td("Distribusi normal dengan mean 0 dan variansi 1."))
                      )
                    ),
                    p(style="margin-top:10px; margin-bottom:0; font-size:13px; opacity:0.9;",
                      strong("Intinya: "), "\u0304X ~ N(\u03BC, \u03C3\u00B2/n) ketika n cukup besar."
                    )
                )
              )
            )
        )
      )
    ),
    
    # ================== TAB 3: DISTRIBUSI AWAL ===================
    tabPanel(
      "Distribusi Awal", icon = icon("chart-area"),
      fluidPage(
        div(class="page-wrap",
            br(),
            sidebarLayout(
              sidebarPanel(
                div(class="retro-card soft retro-pad",
                    h3(style="margin-top:0; font-weight:1000;", "Sumber Data"),
                    p(tags$b("Masukkan data dalam bentuk CSV atau Excel (.xlsx/.xls).")),
                    fileInput("file_data2", "Upload Data (CSV / Excel)", accept = c(".csv", ".xlsx", ".xls")),
                    uiOutput("var_select_ui2"),
                    selectInput("ci_level2", "Tingkat Interval Kepercayaan (%)",
                                choices=c(80,90,95,99), selected=95)
                )
              ),
              mainPanel(
                div(class="panel-split",
                    div(class="panel-head", h3("Preview Data")),
                    div(class="panel-body", DTOutput("data_preview_awal"))
                ),
                br(),
                div(class="panel-split",
                    div(class="panel-head", h3("Statistik Deskriptif Variabel")),
                    div(class="panel-body", DTOutput("summary_awal_dt"))
                ),
                br(),
                div(class="panel-split",
                    div(class="panel-head", h3("Interval Kepercayaan Teoretis")),
                    div(class="panel-body", DTOutput("ci_awal_dt"))
                ),
                br(),
                div(class="panel-split",
                    div(class="panel-head", h3("Interpretasi Interval Kepercayaan")),
                    div(class="panel-body", textOutput("interpretasi_awal"))
                ),
                br(),
                div(class="panel-split",
                    div(class="panel-head", h3("Histogram Distribusi Awal")),
                    div(class="panel-body", plotOutput("hist_awal", height="350px"))
                )
              )
            )
        )
      )
    ),
    
    # ================== TAB 4: SIMULASI CLT ===================
    tabPanel(
      "Simulasi CLT", icon = icon("chart-bar"),
      fluidPage(
        div(class="page-wrap",
            br(),
            sidebarLayout(
              sidebarPanel(
                div(class="retro-card soft retro-pad",
                    h3(style="margin-top:0; font-weight:1000;", "Pengaturan CLT"),
                    p(tags$b("Masukkan data dalam bentuk CSV atau Excel (.xlsx/.xls).")),
                    fileInput("file_data", "Upload Data (CSV / Excel)", accept = c(".csv", ".xlsx", ".xls")),
                    uiOutput("var_select_ui"),
                    numericInput("sample_size", "Ukuran Sampel (n)", 30, min = 2),
                    numericInput("n_sim", "Jumlah Simulasi", 1000, min = 10),
                    selectInput("ci_level", "Tingkat CI (%)", choices=c(80,90,95,99), selected=95),
                    actionButton("run_btn", "Jalankan Simulasi", icon = icon("play"), class="retro-btn", style="width:100%;")
                )
              ),
              mainPanel(
                div(class="panel-split",
                    div(class="panel-head", h3("Preview Data")),
                    div(class="panel-body", DTOutput("data_preview"))
                ),
                br(),
                div(class="panel-split",
                    div(class="panel-head", h3("Ringkasan Data Asli")),
                    div(class="panel-body", DTOutput("summary_data_dt"))
                ),
                br(),
                div(class="panel-split",
                    div(class="panel-head", h3("Ringkasan Hasil CLT")),
                    div(class="panel-body", DTOutput("summary_clt_dt"))
                ),
                br(),
                div(class="panel-split",
                    div(class="panel-head", h3("Interval Kepercayaan Teoretis")),
                    div(class="panel-body", DTOutput("inferensi_ci_dt"))
                ),
                br(),
                div(class="panel-split",
                    div(class="panel-head", h3("Interpretasi CI")),
                    div(class="panel-body", textOutput("interpretasi_ci"))
                ),
                br(),
                div(class="panel-split",
                    div(class="panel-head", h3("Distribusi Rata-rata Sampel (CLT)")),
                    div(class="panel-body", plotOutput("hist_means", height="350px"))
                )
              )
            )
        )
      )
    ),
    
    # ================== TAB 5: TIM (UPDATED) ===================
    tabPanel(
      "Tim Kami", icon = icon("users"),
      fluidPage(
        div(class="page-wrap",
            br(),
            div(class="retro-card soft retro-pad",
                h2(style="margin:0; font-weight:1000;", "Profil Kelompok"),
                p("Berikut adalah profil singkat anggota tim pengembang Aplikasi CLT yang berperan dalam perancangan, pengembangan antarmuka, pengolahan data, serta implementasi konsep Central Limit Theorem ke dalam aplikasi berbasis Shiny")
            ),
            br(),
            
            fluidRow(
              column(4,
                     div(class="retro-card retro-pad team-card",
                         tags$img(src="img/AZFA1.jpeg", class="team-img"),
                         tags$div(style="height:10px;"),
                         div(class="team-body",
                             div(
                               h3(style="margin:0; font-weight:1000;", "Musyaffa' Azfa Rasyid"),
                               p(style="margin:6px 0 0;", "NIM: 112413698"),
                               p(style="margin:6px 0 0; opacity:.9;", "Peran: Frontend & Dokumentasi")
                             ),
                             div(class="team-footer",
                                 tags$a(href="https://instagram.com/azfarasyidd", target="_blank", icon("instagram"), style="margin-right:30px;font-size:26px;")
                             )
                         )
                     )
              ),
              column(4,
                     div(class="retro-card retro-pad team-card",
                         tags$img(src="img/YUMNA1.jpeg", class="team-img"),
                         tags$div(style="height:10px;"),
                         div(class="team-body",
                             div(
                               h3(style="margin:0; font-weight:1000;", "Hanifah Yumna"),
                               p(style="margin:6px 0 0;", "NIM: 112413595"),
                               p(style="margin:6px 0 0; opacity:.9;", "Peran: Backend & UI/UX")
                             ),
                             div(class="team-footer",
                                 tags$a(href="https://instagram.com/hanifah_y.__", target="_blank", icon("instagram"), style="margin-right:30px;font-size:26px;")
                             )
                         )
                     )
              ),
              column(4,
                     div(class="retro-card retro-pad team-card",
                         tags$img(src="img/KUKUH.jpeg", class="team-img"),
                         tags$div(style="height:10px;"),
                         div(class="team-body",
                             div(
                               h3(style="margin:0; font-weight:1000;", "Kukuh Bagus Purnomo S."),
                               p(style="margin:6px 0 0;", "NIM: 112413632"),
                               p(style="margin:6px 0 0; opacity:.9;", "Peran: Backend & Logika Simulasi")
                             ),
                             div(class="team-footer",
                                 tags$a(href="https://instagram.com/_baguzzz.ps", target="_blank", icon("instagram"), style="margin-right:30px;font-size:26px;")
                             )
                         )
                     )
              )
            ),
            
            br(),
            div(class="retro-card soft retro-pad", style="text-align:center;",
                h3(style="margin:0; font-weight:1000;", "Quote / Motto Tim"),
                p(style="font-style:italic; font-size:16px; margin:8px 0 0;",
                  "“Statistik bukan sekadar angka, tetapi cerita di balik data.”"
                )
            )
        )
      )
    )
  )
)

# ================================================================
#                           SERVER
# ================================================================
server <- function(input, output, session) {
  
  send_highlight <- function(id, duration = 2000, shake = FALSE) {
    session$sendCustomMessage("highlight", list(id = id, duration = duration, shake = shake))
  }
  
  # ================== DISTRIBUSI AWAL ==================
  data_awal_df <- reactive({
    req(input$file_data2)
    df <- read_any(input$file_data2$datapath)
    as.data.frame(df)
  })
  
  output$var_select_ui2 <- renderUI({
    df <- data_awal_df()
    num_vars <- names(df)[sapply(df, is.numeric)]
    selectInput("var_name2", "Pilih variabel numerik:", num_vars)
  })
  
  data_awal <- reactive({
    df <- data_awal_df()
    req(input$var_name2)
    x <- df[[input$var_name2]]
    x <- x[!is.na(x)]
    x
  })
  
  output$data_preview_awal <- renderDT({
    df <- data_awal_df()
    datatable(df, options = list(scrollY = "300px", scrollX = TRUE, paging = FALSE))
  })
  
  output$summary_awal_dt <- renderDT({
    x <- data_awal()
    df <- data.frame(
      Statistik = c(
        "Jumlah Data (N)",
        "Rata-rata (Mean)",
        "Nilai Tengah (Median)",
        "Standar Deviasi (SD)",
        "Nilai Minimum (Min)",
        "Nilai Maksimum (Max)"
      ),
      Nilai = c(length(x), mean(x), median(x), sd(x), min(x), max(x))
    )
    datatable(df, options = list(dom = "t", paging = FALSE), rownames = FALSE)
  })
  
  output$ci_awal_dt <- renderDT({
    x       <- data_awal()
    lvl_num <- as.numeric(input$ci_level2) / 100
    alpha   <- 1 - lvl_num
    z       <- qnorm(1 - alpha/2)
    se      <- sd(x) / sqrt(length(x))
    lower   <- mean(x) - z * se
    upper   <- mean(x) + z * se
    
    df <- data.frame(
      `Tingkat CI (%)`   = as.numeric(input$ci_level2),
      `Batas Bawah`      = round(lower, 4),
      `Batas Atas`       = round(upper, 4),
      `Panjang Interval` = round(upper - lower, 4)
    )
    datatable(df, options = list(dom = "t", paging = FALSE), rownames = FALSE)
  })
  
  output$interpretasi_awal <- renderText({
    x    <- data_awal()
    lvl  <- as.numeric(input$ci_level2)
    se   <- sd(x) / sqrt(length(x))
    z    <- qnorm(1 - (1 - lvl/100)/2)
    lower <- mean(x) - z * se
    upper <- mean(x) + z * se
    paste0(
      "Dengan tingkat kepercayaan ", lvl,
      "%, rata-rata populasi diperkirakan berada pada interval [",
      round(lower,4), ", ", round(upper,4), "]."
    )
  })
  
  output$hist_awal <- renderPlot({
    x <- data_awal()
    ggplot(data.frame(x = x), aes(x)) +
      geom_histogram(bins = 30, fill = "#8d493a", alpha = .8) +
      theme_minimal() +
      labs(title = "Histogram Distribusi Awal", x = "Nilai", y = "Frekuensi")
  })
  
  # ================== SIMULASI CLT ==================
  data_reactive <- reactive({
    req(input$file_data)
    df <- read_any(input$file_data$datapath)
    as.data.frame(df)
  })
  
  output$var_select_ui <- renderUI({
    df <- data_reactive()
    num_vars <- names(df)[sapply(df, is.numeric)]
    selectInput("var_name", "Pilih variabel numerik:", num_vars)
  })
  
  output$data_preview <- renderDT({
    datatable(
      data_reactive(),
      options = list(scrollY = "300px", scrollX = TRUE, paging = FALSE)
    )
  })
  
  output$summary_data_dt <- renderDT({
    df <- data_reactive()
    req(input$var_name)
    x <- df[[input$var_name]]
    x <- x[!is.na(x)]
    
    out <- data.frame(
      Statistik = c(
        "Jumlah Data (N)",
        "Rata-rata (Mean)",
        "Nilai Tengah (Median)",
        "Standar Deviasi (SD)",
        "Nilai Minimum (Min)",
        "Nilai Maksimum (Max)"
      ),
      Nilai = c(length(x), mean(x), median(x), sd(x), min(x), max(x))
    )
    datatable(out, options = list(dom = "t", paging = FALSE), rownames = FALSE)
  })
  
  sim_run <- reactiveVal(FALSE)
  
  sim_result <- eventReactive(input$run_btn, {
    df <- data_reactive()
    req(input$var_name)
    x <- df[[input$var_name]]
    x <- x[!is.na(x)]
    
    n       <- input$sample_size
    m       <- input$n_sim
    lvl_num <- as.numeric(input$ci_level) / 100
    alpha   <- 1 - lvl_num
    
    means    <- replicate(m, mean(sample(x, n, replace = TRUE)))
    mean_pop <- mean(x)
    sd_pop   <- sd(x)
    se       <- sd_pop / sqrt(n)
    z        <- qnorm(1 - alpha/2)
    ci       <- c(mean_pop - z * se, mean_pop + z * se)
    
    sim_run(TRUE)
    
    list(
      means    = means,
      mean_pop = mean_pop,
      sd_pop   = sd_pop,
      se       = se,
      ci       = ci,
      lvl      = as.numeric(input$ci_level),
      n        = n,
      m        = m
    )
  })
  
  output$summary_clt_dt <- renderDT({
    r <- sim_result()
    df <- data.frame(
      Statistik = c(
        "Rata-rata Populasi (Mean)",
        "Simpangan Baku Populasi (SD)",
        "Ukuran Sampel (n)",
        "Jumlah Simulasi",
        "Rata-rata dari Rata-rata Sampel",
        "Simpangan Baku Rata-rata Sampel",
        "Galat Baku Teoretis (SE)"
      ),
      Nilai = c(
        r$mean_pop, r$sd_pop, r$n, r$m,
        mean(r$means), sd(r$means), r$se
      )
    )
    datatable(df, options = list(dom = "t", paging = FALSE), rownames = FALSE)
  })
  
  output$inferensi_ci_dt <- renderDT({
    r <- sim_result()
    df <- data.frame(
      `Tingkat CI (%)`   = r$lvl,
      `Batas Bawah`      = round(r$ci[1], 4),
      `Batas Atas`       = round(r$ci[2], 4),
      `Panjang Interval` = round(r$ci[2] - r$ci[1], 4)
    )
    datatable(df, options = list(dom = "t", paging = FALSE), rownames = FALSE)
  })
  
  output$interpretasi_ci <- renderText({
    r <- sim_result()
    paste0(
      "Dengan tingkat kepercayaan ", r$lvl,
      "%, rata-rata populasi diperkirakan berada dalam interval [",
      round(r$ci[1],4), ", ", round(r$ci[2],4), "]."
    )
  })
  
  output$hist_means <- renderPlot({
    r <- sim_result()
    ggplot(data.frame(means = r$means), aes(means)) +
      geom_histogram(bins = 30, fill = "#d0b8a8", alpha = .7) +
      geom_vline(xintercept = r$mean_pop, color = "#8d493a", linetype = "dashed") +
      theme_minimal() +
      labs(
        title    = "Distribusi Rata-rata Sampel (CLT)",
        subtitle = "Garis coklat = rata-rata populasi",
        x        = "Rata-rata Sampel",
        y        = "Frekuensi"
      )
  })
  
  # ================== Interaktivitas tombol langkah di Beranda ==================
  observeEvent(input$step1_box, {
    showModal(modalDialog(
      title = "Upload Data Untuk",
      easyClose = TRUE,
      footer = modalButton("Tutup"),
      tagList(
        p("Pilih target upload:"),
        fluidRow(
          column(6, actionButton("upload_to_dist", "Upload untuk Distribusi Awal", icon = icon("chart-area"), style = "width:100%;")),
          column(6, actionButton("upload_to_sim",  "Upload untuk Simulasi CLT",   icon = icon("chart-bar"),  style = "width:100%;"))
        )
      )
    ))
  })
  
  observeEvent(input$upload_to_dist, {
    removeModal()
    updateTabsetPanel(session, "main_nav", selected = "Distribusi Awal")
    send_highlight("file_data2", duration = 2600, shake = TRUE)
  })
  
  observeEvent(input$upload_to_sim, {
    removeModal()
    updateTabsetPanel(session, "main_nav", selected = "Simulasi CLT")
    send_highlight("file_data", duration = 2600, shake = TRUE)
  })
  
  observeEvent(input$step2_box, {
    updateTabsetPanel(session, "main_nav", selected = "Distribusi Awal")
    if (is.null(input$file_data2)) {
      showNotification("Silakan upload file CSV/Excel terlebih dahulu pada tab Distribusi Awal.", type = "error", duration = 5)
      send_highlight("file_data2", duration = 2600, shake = TRUE)
      return()
    }
    if (is.null(input$var_name2)) {
      showNotification("Silakan pilih variabel numerik sebelum melihat statistik.", type = "warning", duration = 4)
      send_highlight("var_name2", duration = 2400, shake = TRUE)
      return()
    }
    showNotification("Menuju Statistik Deskriptif dan Histogram.", type = "message", duration = 2)
    send_highlight("data_preview_awal", duration = 1800, shake = FALSE)
  })
  
  observeEvent(input$step3_box, {
    updateTabsetPanel(session, "main_nav", selected = "Simulasi CLT")
    if (is.null(input$file_data)) {
      showNotification("Silakan upload file CSV/Excel pada tab Simulasi CLT sebelum menjalankan simulasi.", type = "warning", duration = 5)
      send_highlight("file_data", duration = 2600, shake = TRUE)
      return()
    }
    if (!is.null(input$file_data) && is.null(input$var_name)) {
      showNotification("Silakan pilih variabel numerik untuk simulasi.", type = "warning", duration = 4)
      send_highlight("var_name", duration = 2200, shake = TRUE)
      return()
    }
    showNotification("Silakan atur parameter lalu tekan 'Jalankan Simulasi'.", type = "message", duration = 3)
  })
  
  observeEvent(input$step4_box, {
    showModal(modalDialog(
      title = "Interpretasi Hasil",
      easyClose = TRUE,
      footer = modalButton("Tutup"),
      tagList(
        p("Pilih interpretasi yang ingin Anda lihat:"),
        fluidRow(
          column(6, actionButton("interpret_dist", "Interpretasi Distribusi Awal", icon = icon("chart-area"), style = "width:100%;")),
          column(6, actionButton("interpret_clt",  "Interpretasi CLT",           icon = icon("chart-bar"),  style = "width:100%;"))
        )
      )
    ))
  })
  
  observeEvent(input$interpret_dist, {
    removeModal()
    updateTabsetPanel(session, "main_nav", selected = "Distribusi Awal")
    if (is.null(input$file_data2)) {
      showNotification("Sebelum interpretasi, upload file CSV/Excel pada tab Distribusi Awal.", type = "error", duration = 5)
      send_highlight("file_data2", duration = 2600, shake = TRUE)
      return()
    }
    if (is.null(input$var_name2)) {
      showNotification("Sebelum interpretasi, pilih variabel numerik.", type = "error", duration = 4)
      send_highlight("var_name2", duration = 2600, shake = TRUE)
      return()
    }
    showNotification("Menampilkan interpretasi Distribusi Awal.", type = "message", duration = 2)
    send_highlight("interpretasi_awal", duration = 2200, shake = FALSE)
  })
  
  observeEvent(input$interpret_clt, {
    removeModal()
    updateTabsetPanel(session, "main_nav", selected = "Simulasi CLT")
    if (!isTRUE(sim_run())) {
      showNotification("Simulasi belum dijalankan. Tekan 'Jalankan Simulasi' terlebih dahulu.", type = "error", duration = 5)
      send_highlight("run_btn", duration = 2600, shake = TRUE)
      return()
    }
    showNotification("Menampilkan interpretasi hasil simulasi CLT.", type = "message", duration = 2)
    send_highlight("interpretasi_ci", duration = 2200, shake = FALSE)
  })
  
  observeEvent(input$file_data2, { send_highlight("file_data2", duration = 1400, shake = FALSE) })
  observeEvent(input$file_data,  { send_highlight("file_data",  duration = 1400, shake = FALSE) })
}

shinyApp(ui, server)