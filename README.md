# Tracking_and_forecasting_milepost_moments_of_COVID-19
This is a repository for all data and code used in the paper <Tracking_and_forecasting_milepost_moments_of_the_epidemic_in_the_early_outbreak__framework_and_applications_to_the_COVID_19> .
Link: https://www.medrxiv.org/content/10.1101/2020.03.21.20040139v1.full.pdf+html

## Introduction
In this paper, We proposed a method to predict "turning points", whose main idea is using the change velocity of infection rate (InfectionRateVelocity in code) and the change velocity of completion rate(RemovedRateVelocity in code) to forecast newly diagnoses cases and number of cases treated in the hospital in the future. Thus, we proposed one of the algorithms to calculate the change rate and then make the prediction, which is the method we used in our paper. The original data collected from China CDC is also included.

## Description of Data
The data we used is a csv file with five columns, i.e. date,
         the cumulative confirmed cases up to the given day t,
         the daily confirmed cases at day t, 
         the daily recovered ones, 
         and the daily deaths at day t.
        
It worth noticing that, in the application part of our paper, we only focus on the area "China mainland beyong Hubei Province", thus all the data we provided is from this area, and the date we used is from January 29th to February 29th.

## Overall Description of Code
In our code, there are four functions, i.e. Iconicfun, CalculateVelocity, Prediction and totalPrediction, with a simple example followed.

Iconicfun: Using original data to calculate the iconic indicators we defined in our paper.

CalculateVelocity: Compute the velocity of infection rate change and completion rate change.

Prediction: Predict future infection rate, removed rate, confirmed cases and number of patints treated in hospital of each days.

totalPrediction: Integrate all functions above, and handle a special situation (in case removedrate>1, which is conterintuitive).
