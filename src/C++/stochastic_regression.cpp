// mc_study.cpp
/*
    This program performs a stochastic regression imputation
    for a target variable y with missing values using completely
    observed data X. The calculations are run in serial and then
    in parallel. The parallel implementation is based on C++'s 
    std::thread. For matrix computations the C++ header only 
    library Eigen is used. You need Eigen to compile this code.
    Eigen is free software available at 
    https://eigen.tuxfamily.org/
    
    To comopile this program use:
    $ g++ -I eigen-3.4.0 stochastic_regression.cpp -o stochastic_regression.out -std=c++17 (serial)
    $ g++ -I eigen-3.4.0 stochastic_regression.cpp -o stochastic_regression.out -std=c++17 -lpthread (parallel)
    $ clang++ -I eigen-3.4.0 stochastic_regression.cpp -o stochastic_regression.out -std=c++17 (serial)
    $ clang++ -I eigen-3.4.0 stochastic_regression.cpp -o stochastic_regression.out -std=c++17 -lpthread (parallel)

    To run it type
    $ ./stochastic_regression.out
*/

#include <iostream>
#include <random>
#include <cmath>
#include <string>
#include <vector>
#include <fstream>
#include <thread>
#include <chrono>

#include <Eigen/Dense>
#include <Eigen/QR> 


// Linear model object that can fit data in the form of a target y
// and features X using multiple linear regression. 
class LinearModel
{
public:
    double sigma;
    Eigen::VectorXd beta;
    Eigen::VectorXd predictions;
    Eigen::VectorXd residuals;
    Eigen::MatrixXd design;
    Eigen::MatrixXd inv_matrix_prod;

    LinearModel();
    ~LinearModel();
    void fit(Eigen::MatrixXd X, Eigen::VectorXd y);
};

LinearModel::LinearModel()
{
}

LinearModel::~LinearModel()
{
}

// Fit a linear model of the form y = X * beta + epsilon
// using least squares.
void LinearModel::fit(Eigen::MatrixXd X, Eigen::VectorXd y)
{   
    // Add ones to the model for the intercept parameter.
    // Therefore resize and reshape the matrix X.
    X.conservativeResize(X.rows(), X.cols() + 1);
    for (int i = X.cols() - 1; i > 0 ; i--) 
    {
        X.col(i) = X.col(i - 1);
    }
    X.col(0) = Eigen::VectorXd::Ones(X.rows());
    design = X;

    // Calculate OLS estimates beta.
    Eigen::MatrixXd XX = X.transpose() * X;
    inv_matrix_prod = XX.inverse();
    beta = X.transpose() * y;
    beta = inv_matrix_prod * beta;

    // Calculate the residuals.
    predictions = X * beta;
    residuals = y - predictions;
    Eigen::VectorXd residuals_sq = residuals.array().pow(2);

    // Calculate sigma, that is the variance of the error terms.
    sigma = sqrt(1.0f / (X.rows() - beta.size()) * residuals_sq.sum());
}


// Generates draws from a multivariate normal distribution.
// For reference see:
// https://stackoverflow.com/questions/6142576/sample-from-multivariate-normal-gaussian-distribution-in-c
struct normal_random_variable
{
    normal_random_variable(Eigen::MatrixXd const& covar)
        : normal_random_variable(Eigen::VectorXd::Zero(covar.rows()), covar)
    {}

    normal_random_variable(Eigen::VectorXd const& mean, Eigen::MatrixXd const& covar)
        : mean(mean)
    {
        Eigen::SelfAdjointEigenSolver<Eigen::MatrixXd> eigenSolver(covar);
        transform = eigenSolver.eigenvectors() * eigenSolver.eigenvalues().cwiseSqrt().asDiagonal();
    }

    Eigen::VectorXd mean;
    Eigen::MatrixXd transform;

    Eigen::VectorXd operator()() const
    {
        static std::mt19937 gen{ std::random_device{}() };
        static std::normal_distribution<> dist;

        return mean + transform * Eigen::VectorXd{ mean.size() }.unaryExpr([&](auto x) { return dist(gen); });
    }
};


// Reads a CSV file and pushes its content into an Eigen::MatrixXd.
// For reference see:
// https://stackoverflow.com/questions/34247057/how-to-read-csv-file-and-assign-to-eigen-matrix/39146048
// https://www.py4u.net/discuss/80056
Eigen::MatrixXd read_matrix_csv(const std::string & filepath) 
{
    // Read values form file and write it to buffer vector.
    std::ifstream indata;
    indata.open(filepath);
    std::string line;
    std::vector<double> values;
    uint rows = 0;
    while (std::getline(indata, line)) 
    {
        std::stringstream lineStream(line);
        std::string cell;
        while (std::getline(lineStream, cell, ',')) 
        {
            values.push_back(std::stod(cell));
        }
        ++rows;
    }

    // Detect or set number of rows/columns
    size_t num_rows = rows;
    size_t num_cols = values.size() / rows;

    // Map buffer to Eigen::Matrix and return it.
    Eigen::MatrixXd csv_matrix = Eigen::Map<Eigen::Matrix <double, Eigen::Dynamic, Eigen::Dynamic, Eigen::RowMajor>> (&values.data()[0], num_rows, num_cols);
    return csv_matrix;
}


// Performs stochastic regression imputation on a data vector y
// and observed variables X. 
Eigen::VectorXd stochastic_regression_imputation(Eigen::MatrixXd X_miss, Eigen::MatrixXd X_obs, Eigen::VectorXd y_obs)
{
    int n_obs = X_obs.rows();
    int n_miss = X_miss.rows();

    std::default_random_engine generator;
    std::chi_squared_distribution<double> chi(n_obs - 3); 

    // Fit linear model only on the observed values.
    LinearModel regmod;
    regmod.fit(X_obs, y_obs);
    Eigen::VectorXd beta_hat = regmod.beta;

    // Random draws for sigma^2 from the scaled inverse Chi^2-distribution.
    double sigma_sq_tilde = (n_obs - 3) * pow(regmod.sigma, 2) / chi(generator);

    // Random draws for beta from a mv. normal distribution.
    Eigen::MatrixXd variance = regmod.inv_matrix_prod * sigma_sq_tilde;
    normal_random_variable sample {beta_hat, variance};
    Eigen::VectorXd beta_tilde = sample();
    Eigen::VectorXd means = regmod.design * beta_tilde;

    // Draw the imputations for y from a normal distribution.
    Eigen::VectorXd y_imputation(n_miss);
    for (int i = 0; i < n_miss; i++) 
    {
        std::normal_distribution<double> normal(means[i], sqrt(sigma_sq_tilde));
        y_imputation[i] = normal(generator);
    }

    return y_imputation;
}


// Performs M multiple imputations using stochastic_regression
// and a Bayesian linear model. 
Eigen::MatrixXd multiple_imputation(int num_imp, Eigen::MatrixXd X_miss, Eigen::MatrixXd X_obs, Eigen::VectorXd y_obs) 
{
    Eigen::MatrixXd imputations(X_obs.rows() + X_miss.rows(), num_imp);

    for (int m = 0; m < num_imp; m++)
    {
        // Calculate imputations for y and concantenate to one vector.
        Eigen::VectorXd y_imp = stochastic_regression_imputation(X_miss, X_obs, y_obs);
        Eigen::VectorXd y_complete(y_obs.size() + y_imp.size());
        y_complete << y_obs, y_imp;
        imputations.col(m) = y_complete;
    }

    return imputations;
}
 

int main() 
{   
    // Load the data.
    Eigen::MatrixXd X_miss = read_matrix_csv("data/X_miss.csv");
    Eigen::VectorXd y_miss = read_matrix_csv("data/y_miss.csv");
    Eigen::MatrixXd X_obs = read_matrix_csv("data/X_obs.csv");
    Eigen::VectorXd y_obs = read_matrix_csv("data/y_obs.csv");
    Eigen::MatrixXd imputations;

    // Imputation parameters.
    int num_imp = 10000;

    // Calculate one imputation.
    //Eigen::VectorXd y_imp = stochastic_regression_imputation(X_miss, X_obs, y_obs);
    //std::cout << y_imp << std::endl;

    // Calculate multiple imputations.
    std::chrono::steady_clock::time_point begin = std::chrono::steady_clock::now();
    imputations = multiple_imputation(num_imp, X_miss, X_obs, y_obs);
    //std::cout << imputations << std::endl;

    /*for (int i = 0; i < num_imp; i++)
    {
        std::cout << "Mean of imputation " << i << " is " << imputations.col(i).mean() << std::endl;
    }*/

    std::chrono::steady_clock::time_point end = std::chrono::steady_clock::now();
    std::cout << "(Serial) Runtime = " << std::chrono::duration_cast<std::chrono::microseconds>(end - begin).count() << " [µs]" << std::endl;
    std::cout << "(Serial) Runtime = " << std::chrono::duration_cast<std::chrono::microseconds>(end - begin).count() / 1e6 << " [s]" << std::endl;
    std::cout << std::endl;

    // Parallel version using std::thread. Here the number of
    // multiple imputations is distributed to different threads.
    int num_cores = std::thread::hardware_concurrency();
    begin = std::chrono::steady_clock::now();
    std::vector<std::thread> threads;

    for (int i = 0; i < num_cores; i++) 
    {
        threads.push_back(std::thread(multiple_imputation, num_imp / num_cores, X_miss, X_obs, y_obs));
    }
    
    for (auto &th : threads) 
    {
        th.join();
    }

    end = std::chrono::steady_clock::now();
    std::cout << "Running in on " << num_cores << " core(s)." << std::endl;
    std::cout << "(Parallel) Runtime = " << std::chrono::duration_cast<std::chrono::microseconds>(end - begin).count() << " [µs]" << std::endl;
    std::cout << "(Parallel) Runtime = " << std::chrono::duration_cast<std::chrono::microseconds>(end - begin).count() / 1e6 << " [s]" << std::endl;
    
    return 0;
}
