# ComputationalFinance
Repository for the computational finance module in the Southampton Data Science Masters

[Aims and Objectives](http://www.ecs.soton.ac.uk/module/COMP6212#aims_%26amp%3B_objectives)

Demonstrate knowledge and understanding of: 

 * The concepts underlying computational finance
 * The mathematical tools, and their computational implementations underlying the subject. 
 * Implement a simulated fund management system that uses real-life data from the stock exchange. 

Syllabus:
 
 1. Mathematical preliminaries
	 * Numerical analaysis
	 * Optimization
	 * Stochastic differential equations
	 * Monte-Carlo simulations
 
 2. Software preliminaries
	 * MATLAB
	 * Finance tollbox in MATLAB
	 * Other tools - overview of R and packages

 3. Financial instruments and their uses

 4. Portfolio opimization
 	* Utility theory
 	* Quantifying risk

 5. Options Pricing
 	* Black-Scholes model
 	* Options pricing by Monte Carlo methods


## CW1: Portfolio Optimisation [Specification]() [Report]() (Grade: 30/30)
This cousework aimed to analyse standard methods of producing efficient portfolios:

 * A naive, evenly spread portfolio. 
 * Using the efficient frontier. 
 * A greedy sparse index tracking algorithm. 
 * A regularised sparse index tracking algorithm. 

 Each of these were computed on historic data and the results were compared / critically analysed, discussing where each succeeds and fails as well as how they differ in theory vs. practice. 

## CW2: Options pricing [Specification]() [Report]() (Grade: 30/30)
This coursework looks at computing options pricing using Black-Scholes and Binomial lattice methods. This is compared to real data and the limits of these methods are discussed: observing volatility smiles, comparing the differences between European and American options and methods for estimating volatility.

## CW3 Non-parametric options pricing [Specification]() [Report]() (Grade: 15/15)
In this coursework we consider learning options pricing through a neural network approach. We train historic data using a gaussian mixture model. We discuss the effecitveness of such models and the contrast with parametric simpler models from the previous coursework. 

## CW4 Kalman Filtering and Lasso Regularisation [Specification]() [Report]() (Grade: 25/25)
In this coursework we build a Kalman filter to filter the noise from historic index prices. We investigate the residuals of this filter to link them to econometric variables such as oil price. To do this a lasso regularisation model is built. 