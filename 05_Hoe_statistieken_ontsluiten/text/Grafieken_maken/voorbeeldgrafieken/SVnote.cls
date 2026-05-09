\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{SVnote}

\LoadClass{article}




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% GENERAL PACKAGES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\RequirePackage{graphicx}
\RequirePackage{etoolbox}
\RequirePackage[style={ddmmyyyy},datesep=.]{datetime2}
\RequirePackage{amssymb}

\RequirePackage[dutch]{babel}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% DEFINITIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\newcommand\printtitle{}
\newcommand\printshorttitle{}
\renewcommand\title[2][]{
	\renewcommand\printtitle{#2}
	\ifstrempty{#1}{
		\renewcommand\printshorttitle{#2}
		}{
		\renewcommand\printshorttitle{#1}
		}
	}
\newcommand\printsubtitle{}
\newcommand\subtitle[1]{\renewcommand\printsubtitle{#1}}
\newcommand\printauthor{}
\renewcommand\author[1]{\renewcommand\printauthor{#1}}	



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% COLORS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\RequirePackage{xcolor}
\input{SVnote/Niveau2-Kleurenpalet11.tex}
%\definecolor{textcol}{HTML}{34444A}
%\definecolor{gray1}{HTML}{EDEDED}
%\definecolor{gray2}{HTML}{E5E5E5}
%\definecolor{gray3}{HTML}{D5D5D5}
%\definecolor{gray4}{HTML}{989898}
\color{textcol}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% FONTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\RequirePackage{fontspec}
\RequirePackage{anyfontsize}
\setmainfont{FlandersArtSerif-Regular.otf}[Path=SVnote/
    ,BoldFont = FlandersArtSerif-Bold.otf
    ,AutoFakeSlant=0.4
    ]
\setsansfont{FlandersArtSans-Regular.otf}[Path=SVnote/
    ,BoldFont = FlandersArtSans-Bold.otf
    ,ItalicFont = FlandersArtSans-Italic.otf
    ,BoldItalicFont = FlandersArtSans-BoldItalic.otf 
    ]
\newfontfamily\medsans{FlandersArtSans-Medium.otf}[Path=SVnote/]
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% PAGE LAY-OUT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%   
    
\RequirePackage[]{geometry}
\geometry{
    ,a4paper
    ,twoside
    ,inner=35mm
    ,outer=35mm
    ,tmargin=35mm
    ,bmargin=35mm
    ,footskip=9.5mm
    }
\RequirePackage{parskip}
\widowpenalty=10000
\clubpenalty=10000
\sloppy
    
    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% HEADERS AND FOOTERS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
   
\RequirePackage{fancyhdr}   
\RequirePackage{lastpage}  
\pagestyle{fancy}
\fancyhf{} 
\renewcommand{\headrulewidth}{0pt}
\newcommand\pagenumber{pagina \thepage\ van \pageref{LastPage}}
\newcommand\makefoot[2]{
	\begin{minipage}{\linewidth}
	\scriptsize
	\leavevmode\xleaders\hbox{/\hspace{-.3ex}}\hfill\kern0pt/
	\\[\baselineskip]
	#1\hfill\printshorttitle\hfill#2
	\end{minipage}
	}
\fancyfoot[CE]{\makefoot{\pagenumber}{\DTMtoday}}	
\fancyfoot[CO]{\makefoot{\DTMtoday}{\pagenumber}} 
   
    
    
    
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% TITLE PAGE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
    
\renewcommand\titlepage{
	\newgeometry{tmargin=15mm,bmargin=10mm,hmargin=20mm}
	\thispagestyle{empty}
	{\sffamily\huge STATISTIEK\\[2pt]{\medsans VLAANDEREN}}
	\par
	\vspace{20mm}	
	\par
	\leavevmode\xleaders\hbox{/\hspace{-.3ex}}\hfill\kern0pt/
	\vspace{5mm}
	\begin{center}
	\fontsize{50}{30}\selectfont\sffamily\bfseries
	\MakeUppercase{\printtitle}
	\end{center}
	\vspace{5mm}
	\begin{center}
	\fontsize{25}{30}\selectfont\LARGE\bfseries
	\ifdefempty\printsubtitle{}{\printsubtitle\ /\ }
	\ifdefempty\printauthor{}{\printauthor\ /\ }
	\DTMtoday
	\end{center}
	\vspace{5mm}
	\leavevmode\xleaders\hbox{/\hspace{-.3ex}}\hfill\kern0pt/
	\vfill
	\includegraphics[height=15mm]{SVnote/vlaanderen-white}
	\hfill 
	\begin{minipage}[b][15mm][c]{40mm}
	\href{www.statistiekvlaanderen.be}{www.statistiekvlaanderen.be}
	\end{minipage}
    \restoregeometry
    \newpage
	}
   
   

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% TABLE OF CONTENTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%    
    
\RequirePackage{titletoc}    
\renewcommand\tableofcontents{%
    INHOUD
    \@starttoc{toc}
    \clearpage
    }
    
\dottedcontents{section}[15mm]{\vspace{\baselineskip}}{15mm}{2pt}   
\titlecontents{subsection}[15mm]{\small\color{textcol!80}}{\contentslabel{15mm}}{}{ \hfill\contentspage} 
\titlecontents{subsubsection}[15mm]{\small\color{textcol!60}}{\contentslabel{15mm}}{}{ \hfill\contentspage} 

      
    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% SECTION TITLES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  

\RequirePackage{titlesec}

\titleformat{name=\section}{\bfseries\LARGE\MakeUppercase}{\thesection}{2ex}{}


\newcommand{\sectionbreak}{\vspace{2\baselineskip}}

\titleformat{name=\subsection}{\large\bfseries\MakeUppercase}{\thesubsection}{2ex}{}
\newcommand{\subsectionbreak}{\vspace{\baselineskip}}

\titleformat{name=\subsubsection}{\large}{\thesubsubsection}{2ex}{}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% FOOTNOTES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  

\renewcommand{\@makefntext}[1]{%
	\setlength{\parindent}{0pt}%
	\begin{list}{}{%
		\setlength{\labelwidth}{1.5em}%
		\setlength{\leftmargin}{\labelwidth}%
		\setlength{\labelsep}{3pt}%
		\setlength{\itemsep}{0pt}%
		\setlength{\parsep}{0pt}%
		\setlength{\topsep}{0pt}%
%		\setlength{\rightmargin}{0.2\textwidth}%
		\footnotesize%
		}%
	\item[\@makefnmark\hfil]#1%
	\end{list}%
	}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% LISTS    		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\RequirePackage{enumitem}

\setlist{labelindent=0pt,leftmargin=3ex,labelsep=*,align=left}

\setlist[itemize,1]{label=$\blacktriangleright$}
\setlist[itemize,2]{label=$>$}
\setlist[itemize,3]{label=$+$}

\setlist[enumerate,1]{label=\arabic*}
\setlist[enumerate,2]{label=\alph*}
\setlist[enumerate,3]{label=\roman*}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% FLOATS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% \usepackage{rotating}
\usepackage{floatrow}
\usepackage{caption}	
\floatsetup{font=sf,captionskip=.2\baselineskip,justification=raggedright,capposition=top}
% \floatsetup[table]{}
% \floatsetup[figure]{}
% \renewcommand{\thetable}{\arabic{chapter}.\arabic{table}.}  
\captionsetup{font={sf,scriptsize}
             ,labelfont=
             ,justification=justified
             ,singlelinecheck=off
             ,labelsep=colon
             } 
    
    
    
    




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% TABULARS   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\usepackage{tabularx}
\newcolumntype{L}{>{\raggedright\arraybackslash}X}
\newcolumntype{R}{>{\raggedleft\arraybackslash}X}
\newcolumntype{C}{>{\centering\arraybackslash}X}

%\usepackage{tabularray}







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% BIBLIOGRAPHY
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% \RequirePackage[backend=bibtex]{biblatex}
    
    



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% HYPERREFS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\RequirePackage[hidelinks]{hyperref}



