Doctoral Project: Translational Specialistic Medicine "G.B. Morgagni" (Curriculum: Biostatistics and Clinical Epidemiology), University of Padua.  
Candidate: Giovanni Francisco Guevara Vásquez (MD, MPH, PhD Candidate).  
Supervisory Team: Prof. Dario Gregori (Coordinator), Dra. Lorenzoni (Tutor).  
Local Collaborators (El Salvador): Dr. Pablo Salazar-Colocho (Local Tutor), Dr. David Tejada-Peña (Research Team).

Project Overview & TitleFrom Static Estimates to Dynamic Precision: A Hybrid AI Framework (LLM-ML) for Simulating the Health and Economic Impact of Emerging Anti-Sugar Policies in Data-Scarce Settings.  This repository hosts the computational pipelines, systematic review protocols, and data infrastructure for a triennial doctoral research initiative. The project aims to modernize burden-of-disease modeling by transitioning from traditional, static Comparative Risk Assessment (CRA) methods—previously established in El Salvador in 2020 (attributing over 500 deaths and USD 69.35 million in direct costs to sugar-sweetened beverages [SSBs])—to an advanced, hybrid Intelligent Epidemiological Pipeline powered by Large Language Models (LLMs) and Machine Learning (ML) agent-based microsimulation.  

Triennial Research Pipeline
The doctoral pathway is systematically structured into three core milestones:
Year 1: Methodological Foundation and Evidence SynthesisFocus: Systematic mapping of quantitative methodologies for identifying, cleaning, and redistributing mortality "garbage codes" (GCs) and ill-defined causes of death in Low- and Middle-Income Countries (LMICs).
Deliverable: A JBI-compliant, PRISMA-ScR-optimized scoping review protocol executed programmatically within the R statistical environment using litsearchr, synthesisr, and revtools.  
Year 2: Harmonization, Data Quality, and National ApplicationFocus: Acquisition and cleaning of official micro-level cause-specific mortality datasets from the Ministry of Health of El Salvador.
Deliverable: Application of data quality appraisal tools (ANACONDA/VSPI) and advanced redistribution algorithms to recalculate corrected Population Attributable Fractions (PAFs) for metabolic and dietary risk factors.  
Year 3: Dynamic Microsimulation and Policy ForecastingFocus: Development and validation of a hybrid AI framework combining automated parameterization via fine-tuned LLMs (e.g., BioBERT, Llama 3) for "Living Systematic Reviews" with Agent-Based Microsimulation (ABM) using Machine Learning.  
Deliverable: A dynamic policy-simulator dashboard projecting individual-level behavioral adaptations (substitution effects) and economic outcomes under emerging U.S.-inspired regulatory scenarios ("Food as Medicine", front-of-package labeling). 

Reproducibility and Technical Environment
Statistical Environment: R (v4.5+) for evidence synthesis, bibliographic extraction, and quantitative data harmonization.
Automation & Text Mining: rentrez, rscopus, litsearchr, synthesisr, revtools (LDA topic modeling).
Simulation Framework: NetLogo / R-based agent architectures, leveraging principles from Railsback & Grimm (2019) for structural realism and pattern-oriented modeling (POM).

Contributors & Collaborators
Giovanni Francisco Guevara Vásquez (Principal Investigator / PhD Candidate)
Prof. Dario Gregori (PhD Program Coordinator & Tutor, University of Padua)
Dra. Lorenzoni (Co-Tutor, University of Padua)
Dr. Pablo Salazar-Colocho (Local Tutor, Universidad Dr. José Matías Delgado, El Salvador)
Dr. David Tejada-Peña (Research Team, National Health Institute, El Salvador)  

License & Open Science
In alignment with open science commitments, all extraction schemas, scripts, and non-confidential data dictionaries are made publicly available via the Open Science Framework (OSF) and GitHub upon study completion.
