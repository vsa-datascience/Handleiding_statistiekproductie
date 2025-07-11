\NeedsTeXFormat{LaTeX2e}
\ProvidesClass{handleiding_statistiekproductie}

\LoadClass[a4paper,11pt,openany]{book}
\providecommand\maindir{./}





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% SUBFILES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\RequirePackage{subfiles}
\newcommand\includesubfile[2]{{
	\providecommand\subdir{#1}
	\subfile{#1/#2}
	}}







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% SUPPORTING PACKAGES
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\RequirePackage{etoolbox}

%\RequirePackage{etoolbox}
%\RequirePackage{environ}
%\RequirePackage{graphicx}
%\RequirePackage{tikz}
%\RequirePackage[framemethod=TikZ]{mdframed}
%\RequirePackage{amsmath}
%\RequirePackage{afterpage}











%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% LANGUAGE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\RequirePackage[dutch]{babel}






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% PAGE LAY-OUT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\RequirePackage[]{geometry}
\geometry{
	a4paper,
	twoside,
	inner=35mm,
	outer=35mm,
	tmargin=35mm,
	bmargin=35mm,
	footskip=2\baselineskip
	}
\RequirePackage{parskip}
\widowpenalty=10000
\clubpenalty=10000
\sloppy

\setlength{\textfloatsep}{30pt plus 2.0pt minus 4.0pt}
\setlength{\floatsep}{30pt plus 2.0pt minus 2.0pt}   
\setlength{\intextsep}{30pt plus 2.0pt minus 2.0pt}  







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%	 FONTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\RequirePackage{fontspec}
\RequirePackage{anyfontsize}

\setmainfont[
Path = \maindir/handleiding_statistiekproductie/fonts/, % pas dit aan aan jouw mapstructuur
	Extension = .otf,
	UprightFont = *-Regular,
	BoldFont = *-Bold,
	ItalicFont = *-Italic, 
	AutoFakeSlant=0.4,
	FontFace = {l}{n}{*-Light},
	FontFace = {m}{n}{*-Medium},
	FontFace = {k}{n}{*-Thin},
	FontFace = {b}{n}{*-Black},
	FontFace = {c}{n}{*-CondensedRegular},
	FontFace = {cl}{n}{*-CondensedLight},
	FontFace = {cb}{n}{*-CondensedBold}
	]{FlandersArtSerif}
\setsansfont[
	Path = \maindir/handleiding_statistiekproductie/fonts/,
	Extension = .otf,
	UprightFont = *-Regular,
	BoldFont = *-Bold,
	ItalicFont = *-Italic, 
	AutoFakeSlant=0.4,
	FontFace = {l}{n}{*-Light},
	FontFace = {m}{n}{*-Medium},
	FontFace = {k}{n}{*-Thin},
	FontFace = {b}{n}{*-Black},
	FontFace = {c}{n}{*-CondensedRegular},
	FontFace = {cl}{n}{*-CondensedLight},
	FontFace = {cb}{n}{*-CondensedBold}
	]{FlandersArtSans}

\newcommand\lightseries{\fontseries{l}\selectfont}
\newcommand\thinseries{\fontseries{k}\selectfont}
\newcommand\blackseries{\fontseries{b}\selectfont}
\newcommand\mediumseries{\fontseries{m}\selectfont}
\newcommand\condseries{\fontseries{c}\selectfont}
\newcommand\condlightseries{\fontseries{cl}\selectfont}
\newcommand\condboldseries{\fontseries{cb}\selectfont}

\newcommand\textlight[1]{{\fontseries{l}\selectfont #1}}
\newcommand\textthin[1]{{\fontseries{k}\selectfont #1}}
\newcommand\textblack[1]{{\fontseries{b}\selectfont #1}}
\newcommand\textmedium[1]{{\fontseries{m}\selectfont #1}}
\newcommand\textcond[1]{{\fontseries{c}\selectfont #1}}
\newcommand\textcondlight[1]{{\fontseries{cl}\selectfont #1}}
\newcommand\textcondbold[1]{{\fontseries{cb}\selectfont #1}}







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% COLORS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\RequirePackage{xcolor}
\definecolor{maincol1}{HTML}{4B6C7A}
\definecolor{maincol2}{HTML}{71A3B8}
\definecolor{maincol3}{HTML}{8ED2EE}
\definecolor{textcol}{HTML}{34444A}

\definecolor{palgray1}{HTML}{EDEDED}
\definecolor{palgray2}{HTML}{E5E5E5}
\definecolor{palgray3}{HTML}{D5D5D5}
\definecolor{palgray4}{HTML}{989898}

\definecolor{pal1col1}{HTML}{4E7E71}
\definecolor{pal1col2}{HTML}{75C4AF}
\definecolor{pal1col3}{HTML}{79D9BF}

\definecolor{pal2col1}{HTML}{795032}
\definecolor{pal2col2}{HTML}{A66E45}
\definecolor{pal2col3}{HTML}{D18955}

\definecolor{pal3col1}{HTML}{767344}
\definecolor{pal3col2}{HTML}{A5A059}
\definecolor{pal3col3}{HTML}{C1BB67}

\color{textcol}










%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% HEADERS AND FOOTERS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%\RequirePackage{fancyhdr}   
%\pagestyle{fancy}
%\fancyhf{} 
%\renewcommand{\headrulewidth}{0pt}
%\renewcommand{\footrulewidth}{1pt}
%\patchcmd{\footrule}{\if@fancyplain}{\color{maincol2}\if@fancyplain}{}{}
%\fancyfoot[L]{\footnotesize\printshorttitle}
%\fancyfoot[R]{\footnotesize\thepage} 




\RequirePackage{fancyhdr}   
\renewcommand{\headrulewidth}{0pt}   
\def\thefooters{
	\fancyhf{}  
	\fancyfoot{%
		\begin{minipage}{\textwidth}
			\raggedleft%
			\textcolor{maincol1}{\hrulefill}\\[-3pt]
			\textcolor{maincol1}{\scriptsize \thepage}
		\end{minipage}%
		}         
	}
\def\theheaders{
	\fancyhead{%
		\begin{minipage}[b][25pt]{\textwidth}
			\raggedleft\scriptsize\color{maincol1}
			\leftmark\mbox{}\\[-6pt]\hrulefill
		\end{minipage}%
		}          
	} 
\pagestyle{fancy}  
\renewcommand{\chaptermark}[1]{\markboth{\thechapter. #1}{}} 
\thefooters
\theheaders
\fancypagestyle{plain}{\thefooters}   









%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% TITLE PAGE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%\def\printtitle{\mbox{}}
%\def\printshorttitle{\mbox{}}
%\renewcommand\title[2][]{
%	\ifstrempty{#1}{
%		\gdef\printtitle{#2}
%		\gdef\printshorttitle{#2}
%	}{
%		\gdef\printtitle{#2}
%		\gdef\printshorttitle{#1}
%	}
%}
%\def\printsubtitle{\mbox{}}	
%\def\subtitle#1{\def\printsubtitle{#1}}
%\def\printauthor{\mbox{}}
%\def\author#1{\def\printauthor{#1}}	
%\def\printlastpageauthor{\printauthor}
%\def\lastpageauthor#1{\def\printlastpageauthor{#1}}	
%\def\printdate{\today}
%\def\date#1{\def\printdate{#1}}	
%
%\def\maketitlepage{
%	\clearpage
%	\thispagestyle{empty}%
%	\pagecolor{VUBblue}%
%	\strut\vfill 
%	\parbox[t][.6\paperheight]{10.5cm}{
%		\color{white}
%		\begin{flushleft}
%			\titlefont\Huge\MakeUppercase{\printtitle}
%		\end{flushleft}
%		\ifdefempty{\printsubtitle}{}{%
%			\vspace{20pt}
%			\begin{flushleft}
%				\LARGE\printsubtitle
%			\end{flushleft}
%		}	
%		\ifdefempty{\printauthor}{}{
%			\vspace{20pt}
%			\begin{flushleft}
%				\large\printauthor
%			\end{flushleft}
%		}			
%		\ifdefempty{\printdate}{}{
%			\vspace{20pt}
%			\begin{flushleft}
%				\large\printdate
%			\end{flushleft}
%		}	
%	}
%	\hspace{7pt}
%	\makebox[0pt][l]{\raisebox{-\height}{%
%			\includegraphics[height=.6\paperheight]{VUBbook/VUBtriangle}%
%	}}
%	\newline
%	\setlength{\fboxsep}{0pt}
%	\hspace*{-1.3in}%
%	\makebox[0pt][l]{\raisebox{-\height}[0pt][0pt]{%
%			\colorbox{white}{%
%				\begin{minipage}[c][1.3in][c]{\paperwidth}
%					\hspace{1.22in}%
%					\includegraphics[height=0.08\paperwidth]{VUBbook/VUB}
%				\end{minipage}%
%		}}
%	}
%	\clearpage
%	\nopagecolor
%}
%
%\def\makelastpage{
%	\newpage
%	\ifodd\value{page}\hbox{}\newpage\fi
%	\thispagestyle{empty}%
%	\pagecolor{VUBblue}%
%	\strut\vfill 
%	\setlength{\fboxsep}{0pt}
%	\hspace*{-1.3in}%
%	\makebox[0pt][l]{\raisebox{-\height}[0pt][0pt]{%
%			\colorbox{white}{%
%				\begin{minipage}[c][1.3in][c]{\paperwidth}
%					\hspace{1.22in}%
%					\includegraphics[height=0.08\paperwidth]{VUBbook/VUB}
%				\end{minipage}%
%			}
%	}}
%	\clearpage
%	\nopagecolor  
%}
%













%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%  
%%%% SECTION TITLES   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%\RequirePackage[explicit,newparttoc]{titlesec}	
%
%\NewEnviron{parttitlepage}{
%	\thispagestyle{empty}%
%	\pagecolor{VUBblue}%
%	\strut\vfill 
%	\parbox[t][.6\paperheight]{10.5cm}{
%		\color{white}%
%		\BODY	
%	}
%	\makebox[0pt][l]{\raisebox{-\height}{%
%			\hspace{138pt}%
%			\includegraphics[height=.2\paperheight]{VUBbook/VUBtriangle}%
%	}}   
%}
%
%\titleformat{name=\part}{}{}{0pt}{%
%	\begin{parttitlepage}
%		\begin{flushleft}
%			\Large DEEL \thepart
%		\end{flushleft}
%		\vspace{1cm}
%		\begin{flushleft}
%			\titlefont\Huge #1
%		\end{flushleft}
%	\end{parttitlepage}%
%}[\clearpage\nopagecolor]
%
%\titleformat{name=\part,numberless}[display]{}{}{0pt}{%
%	\begin{parttitlepage}
%		\begin{flushleft}
%			\titlefont\Huge #1
%		\end{flushleft}
%	\end{parttitlepage}%                                                                                                
%}[\clearpage\nopagecolor]
%
%\titleformat{name=\chapter}{\titlefont\huge\color{primcol}}{\textcolor{accentcol}{\thechapter.}}{1ex}{\MakeUppercase{#1}}
%\titleformat{name=\section}{\Large\bfseries\color{primcol}}{\textcolor{accentcol}{\thesection}}{1ex}{#1}
%\titleformat{name=\subsection}{\itshape\bfseries\color{primcol}}{\textcolor{accentcol}{\thesubsection}}{1ex}{#1}






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% TABLE OF CONTENTS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%\RequirePackage{titletoc}
%\setcounter{tocdepth}{1}
%
%%\titlecontents{part}[3.8em]{}{\contentslabel{2.3em}}{\hspace*{-2.3em}}{ \textcolor{primcol}{\hrulefill} \contentspage}
%%\titlecontents{section}[3.8em]{}{\contentslabel{2.3em}}{\hspace*{-2.3em}}{ \textcolor{lightgraycol}{\hrulefill} \contentspage}
%\titlecontents{part}[2.3em]{\addvspace{\baselineskip}\bfseries\color{primcol}}{\contentslabel{2.3em}}{\hspace{-2.3em}}{\enspace\hrulefill \contentspage}[\addvspace{.5\baselineskip}]
%\titlecontents{chapter}[4.6em]{}{\contentslabel{2.3em}}{}{\hfill\contentspage}
%
%
%\titlecontents{figure}[3.5em]{\smallskip}{\contentslabel{3.5em}\hspace*{-1em}}{}{\hspace{1ex}\textcolor{lightgraycol}{\hrulefill}\contentspage}
%\titlecontents{table}[3.5em]{\smallskip}{\contentslabel{3.5em}\hspace*{-1em}}{}{\hspace{1ex}\textcolor{lightgraycol}{\hrulefill}\contentspage}
%
%%%% CORRECT LIJST VAN TABELLEN/FIGUREN 
%%\makeatletter
%%\renewcommand*{\ext@figure}{lot}%
%%\let\c@figure\c@table%
%%\let\ftype@figure\ftype@table%
%%\let\listoftableandfigures\listoftables%
%%\AtBeginDocument{
%	%  \renewcommand{\listtablename}{Lijst van tabellen en figuren}
%	%  }
%%\makeatother






























%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% LISTS    		
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%\RequirePackage{enumitem}
%\setlist[itemize,1]{label=\textcolor{primcol}{$\bullet$}}
%\setlist[itemize,2]{label=\textcolor{accentcol}{$\bullet$}}
%\setlist[itemize,3]{label=$\bullet$}
%\setlist[enumerate,1]{label=\textcolor{primcol}{\arabic{enumi}.}}
%\setlist[enumerate,2]{label=\textcolor{accentcol}{\arabic{enumii}.}}











    




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% BIBLIOGRAPHY
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%\RequirePackage[style=authoryear,backend=bibtex]{biblatex}
%\addbibresource{\maindir/referenties.bib} 









%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% TABULARS   
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%\usepackage{tabularx}
%\newcolumntype{L}{>{\raggedright\arraybackslash}X}
%\newcolumntype{R}{>{\raggedleft\arraybackslash}X}
%\newcolumntype{C}{>{\centering\arraybackslash}X}








%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%% HYPERREFS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%\RequirePackage[hidelinks]{hyperref}





