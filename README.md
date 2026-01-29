# 🧬 DNA Molar Pooling Calculator

### [🚀 Click here to use the Live App](https://danielsangarci.shinyapps.io/dna-molar-pooling-calculator/)

**A precision R Shiny tool for normalizing and pooling double-stranded DNA (dsDNA) based on molarity, not just mass.**

## 📖 Overview

In molecular biology—whether for Next-Generation Sequencing (Nanopore, Illumina), Gibson Assembly, or PCR—**molecule count matters more than weight.** Mixing samples based solely on nanograms often leads to uneven representation, especially when fragments vary in size.

The **DNA Molar Pooling Calculator** automates the physics of DNA normalization. It calculates exactly how many microliters of each sample are required to achieve a target molarity (fmol), ensuring perfectly balanced pools.

## ✨ Key Features

* **🧪 Universal Application:** Works for **any dsDNA** workflow (NGS Library Prep, Plasmid Cloning, PCR Pooling, Gene Assembly).
* **⚡ Interactive Single Mode:** Visual interface for quick calculations of small sample sets with variable mixing ratios.
* **📊 Batch Processing:** Copy-paste directly from Excel. Capable of processing 96-well plates or large datasets instantly.
* **🛡️ Quality Control:**
* **Automatic Safety Checks:** Instantly flags samples with insufficient concentration (Red rows).
* **Status Indicators:** Global alerts (Green/Orange) to ensure no bad samples slip through.


* **📥 Export Ready:** Copy results to clipboard or download as CSV/Excel for your lab notebook or robot.

## 🧮 The Science

This calculator uses the standard average molecular weight for double-stranded DNA base pairs (**650 Daltons**).

* **Inputs:** Amplicon Size (bp), Concentration (ng/µL), Target Amount (fmol), Target Volume (µL).
* **Outputs:** Required Volume (µL) and Water (µL) to reach equimolarity.

## 🚀 How to Run Locally

If you prefer to run this app on your own computer using RStudio:

1. **Clone this repository:**
```bash
git clone https://github.com/danielsangarci/dna-molar-pooling-calculator.git

```


2. **Open the project** in RStudio.
3. **Install Dependencies:**
The app automatically checks for and installs missing packages upon the first run, but you can install them manually:
```r
install.packages(c("shiny", "bslib", "dplyr", "DT"))

```


4. **Run the App:**
Open `app.R` and click the **Run App** button, or run:
```r
shiny::runApp()

```



## 🛠️ Built With

* **R** - Core Logic
* **R Shiny** - Web Framework
* **bslib** - Modern UI/UX (Minty Theme)
* **DT** - Interactive Data Tables

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](https://www.google.com/search?q=LICENSE) file for details.

---

*Created by [Daniel Sánchez García*](https://www.google.com/search?q=https://danielsangarci.shinyapps.io/)
