# CMJ Velocity Prediction Model: XGBoost with SHAP Analysis - Comprehensive Performance and Feature Importance Analysis

## IMPORTANT DISCLAIMER

**This analysis is based exclusively on Countermovement Jump (CMJ) data and reflects lower body power, reactive strength, and movement efficiency only. These predictions DO NOT account for upper body strength, arm strength, or throwing-specific biomechanics, which are also critical components of throwing velocity. The predicted velocity represents the athlete's capacity based on lower body force production characteristics and should be interpreted as one component of overall throwing ability, not a complete assessment of pitching potential.**

## Model Architecture & Methodology

### Model Configuration
- **Algorithm**: XGBoost with optimized hyperparameters
- **Regularization**: L1 (alpha=15) and L2 (lambda=12) penalties
- **Feature Selection**: Correlation cutoff (0.95) and variance cutoff (0.928)
- **Training Enhancement**: Gaussian noise injection (20% of feature standard deviation)
- **Cross-Validation**: 5-fold athlete-grouped stratification

---

## Model Performance Analysis

### Overall Model Performance
- **Correlation**: 0.762 (strong positive relationship)
- **R²**: 0.581 (explains 58.1% of velocity variance)
- **RMSE**: 4.23 mph
- **Sample Size**: 1,160 complete cases
- **Velocity Range**: 57.8 - 98.2 mph (actual), 62.8 - 89.5 mph (predicted)

### Power/Velocity Quartile Analysis

| Power/Velocity Group | N | Mean Actual (mph) | Mean Predicted (mph) | Difference (mph) |
|---------------------|---|-------------------|---------------------|------------------|
| P1_V1 (Low Power, Low Velocity) | 189 | 72.8 | 74.9 | +2.1 |
| P1_V2 (Low Power, Low-Med Velocity) | 82 | 80.4 | 77.5 | -2.9 |
| P1_V3 (Low Power, Med-High Velocity) | 18 | 85.1 | 78.8 | -6.3 |
| P1_V4 (Low Power, High Velocity) | 1 | 88.0 | 79.4 | -8.6 |
| P2_V1 (Low-Med Power, Low Velocity) | 71 | 75.3 | 80.8 | +5.5 |
| P2_V2 (Low-Med Power, Low-Med Velocity) | 97 | 81.0 | 81.3 | +0.4 |
| P2_V3 (Low-Med Power, Med-High Velocity) | 93 | 85.2 | 81.9 | -3.3 |
| P2_V4 (Low-Med Power, High Velocity) | 29 | 88.2 | 81.8 | -6.4 |
| P3_V1 (Med-High Power, Low Velocity) | 26 | 75.6 | 82.2 | +6.6 |
| P3_V2 (Med-High Power, Low-Med Velocity) | 85 | 81.0 | 84.5 | +3.5 |
| P3_V3 (Med-High Power, Med-High Velocity) | 97 | 85.8 | 85.1 | -0.7 |
| P3_V4 (Med-High Power, High Velocity) | 82 | 90.0 | 85.7 | -4.3 |
| P4_V1 (High Power, Low Velocity) | 7 | 76.5 | 85.3 | +8.7 |
| P4_V2 (High Power, Low-Med Velocity) | 24 | 80.7 | 85.2 | +4.5 |
| P4_V3 (High Power, Med-High Velocity) | 83 | 85.6 | 87.3 | +1.7 |
| P4_V4 (High Power, High Velocity) | 176 | 90.4 | 87.6 | -2.8 |


## SHAP Contribution Analysis

### SHAP Contribution Distributions

SHAP contribution percentages show how much each movement phase is responsible for your individual velocity prediction. 

**Why Target the Elite Distribution**: Analysis of 142 athletes who throw 90+ mph (vs. overall dataset average) reveals the optimal biomechanical pattern. Elite throwers show significantly different patterns in key phases compared to the general population.

**Elite vs Bottom 10% Performance Comparison**: The contrast between top and bottom performers reveals critical biomechanical differences:

| Phase | Elite (Top 10%) | Bottom 10% | Difference | Interpretation |
|-------|----------------|------------|------------|----------------|
| **Concentric** | 35.8% | 42.0% | -6.2% | Bottom performers over-rely on raw power |
| **Eccentric** | 18.4% | 15.8% | +2.6% | Elite have better loading capacity |
| **Landing** | 12.1% | 8.6% | +3.5% | Elite have superior stability and control |
| **Braking** | 5.8% | 5.1% | +0.7% | Elite more efficient in transitions |
| **Takeoff** | 4.1% | 4.8% | -0.7% | Elite waste less energy in final phase |


**Training Logic**: Target the elite distribution (17.9% eccentric, 36.4% concentric, 5.7% braking, 11.6% landing, 4.3% takeoff). If you're far from these targets, that indicates the phase most likely to improve your velocity potential.

**How to Interpret the Scores**: These percentages always add up to 100% for each athlete, representing the complete breakdown of what drives their velocity prediction. A high percentage in one area means that particular movement phase is the primary driver of their predicted velocity, while low percentages indicate that phase is less influential for that specific athlete.

**Concentric Contribution (Average 36%)**: This represents the upward, power-generating phase of the jump. Athletes with high concentric contributions (>42%) are "power throwers" whose velocity prediction is primarily driven by their ability to generate explosive force during the upward movement. These athletes typically have strong leg muscles and excel at rapid force production. A low concentric contribution (<30%) suggests the athlete may need significant power development training to improve their velocity potential.

**Eccentric Contribution (Average 19%)**: This represents the downward, loading phase where the athlete controls their descent and stores elastic energy. Athletes with high eccentric contributions (>22%) demonstrate excellent movement control and force absorption capacity. Their ability to efficiently load and control the downward phase is a major factor in their velocity prediction. Interestingly, there's a strong negative correlation (-0.421) between eccentric and concentric contributions, meaning athletes tend to be stronger in one phase or the other, rarely both equally.

**Landing Contribution (Average 11%)**: This represents how well an athlete absorbs and redirects force upon landing. Athletes with high landing contributions (>13%) have superior force absorption mechanics and stability. 

**Braking Contribution (Average 6%)**: This represents the transition period where the athlete stops their downward movement and begins to reverse direction. Athletes with high braking contributions (>8%) excel at this critical transition moment. The strong positive correlation (0.684) between braking and eccentric contributions shows these phases work together - athletes good at controlling their descent are typically also good at the transition point.

**Takeoff Contribution (Average 5%)**: This represents the final explosive phase where the athlete leaves the ground. While it has the lowest average contribution, athletes with high takeoff contributions (>6%) demonstrate exceptional final force application and coordination.

**Practical Interpretation**: An athlete with 45% concentric, 15% eccentric, 12% landing, 5% braking, and 3% takeoff contributions would be classified as a power-dominant athlete who relies heavily on explosive upward force generation but may benefit from improving their loading and control phases. Conversely, an athlete with 25% concentric, 35% eccentric, 15% landing, 12% braking, and 8% takeoff would be more balanced but may need to focus on converting their excellent control into explosive power output.



## Model Validation and Reliability

### Prediction Accuracy Analysis
- **Median Absolute Error**: 2.95 mph
- **75th Percentile Error**: 4.85 mph
- **Athletes Over-Predicted >5mph**: 136 cases (average actual: 74.6 mph, predicted: 81.9 mph)
- **Athletes Under-Predicted >5mph**: 140 cases (average actual: 88.3 mph, predicted: 81.5 mph)

### Power/Velocity Quartile Analysis
**Systematic Patterns**:
- **P1_V4 (Low Power, High Velocity)**: Model under-predicts by 8.6 mph (suggests upper body dominance)
- **P4_V1 (High Power, Low Velocity)**: Model over-predicts by 8.7 mph (suggests mechanical limitations)
- **Balanced Groups (P2_V2, P3_V3)**: Best prediction accuracy

---

## Interpretation Range Methodology

**Range Determination**: All interpretation ranges are based on actual data distributions from 1,160 CMJ assessments:

### Velocity Capacity Ranges
**Based on predicted velocity quartiles:**
- **Low**: <79 mph (bottom 25%)
- **Moderate**: 79-83 mph (25th-50th percentile)
- **Good**: 83-87 mph (50th-75th percentile)  
- **Elite**: >87 mph (top 25%)

### SHAP Contribution Ranges
**Based on actual contribution distributions using 25th/75th percentiles:**

---

## Individual Test Interpretation Guide

### What Each Metric Means for an Athlete

#### 1. **Predicted Max Velocity (Lower Body Capacity)**
The model's estimate of fastball velocity potential based solely on lower body force plate metrics from CMJ testing.

#### 2. **Eccentric Contribution Percentage**
**What it represents**: Proportion of velocity prediction driven by eccentric (lowering/braking) phase metrics.

#### 3. **Concentric Contribution Percentage**
Proportion of velocity prediction driven by concentric (accelerating) phase metrics.

#### 4. **Braking Contribution Percentage**
Proportion driven by braking phase (eccentric-to-concentric transition) metrics.

#### 5. **Landing Contribution Percentage**
Proportion driven by landing mechanics and force absorption.

#### 6. **Takeoff Contribution Percentage**
Proportion driven by takeoff phase force application.
