#include <Rcpp.h>
#include <iterator>
#include <map>
#include <set>
#include <vector>

// [[Rcpp::plugins("cpp11")]]


// Window class
template<class SlideBoundary>
class Window {

public:
    
    class Iterator: public std::iterator<std::input_iterator_tag, double> {
        
    protected:
        const Window& _w;
        int _k;
        
    public:
        Iterator(const Window& w):
        _w(w), _k(0) {};
        
        Iterator(const Iterator& it):
            _w(it._w), _k(it._k) {};
        
        Iterator(const Window& w, const int k):
            _w(w), _k(k) {};
        
        Iterator& operator++() {
            ++_k;
            return *this;
        };
        
        Iterator operator++(int) {
            Iterator it(*this); 
            operator++(); 
            return it;
        };
        
        bool operator==(const Iterator& it) const {
            return _k == it._k;
        };
        
        bool operator!=(const Iterator& it) const {
            return _k != it._k;
        };
        
        const double operator*() const {
            return _w[_k];
        };
        
        void update_end() {
            _k = _w.size();
        };
    };
    
protected:
    
    class Anchor {
        
    protected:
        Window& _w;
        int _row;
        int _col;
        int _nrow;
        int _ncol;
        
    public:
        Anchor(Window& w):
        _w(w), _row(w._row), _col(w._col), _nrow(w._nrow), _ncol(w._ncol) {};
        
        Anchor(const Anchor& s):
            _w(s._w) {};
        
        const int& row() const {
            return _row;
        };
        
        const int& col() const {
            return _col;
        };
        
        const int& nrow() const {
            return _nrow;
        };
        
        const int& ncol() const {
            return _ncol;
        };
        
        const int size() const {
            return _nrow * _ncol;
        };
        
        void position(const int row, const int col) {
            
            _row = row;
            _col = col;
            _w.update_view();
        };
        
        void resize(const int nrow, const int ncol) {
            
            _nrow = nrow;
            _ncol = ncol;
            _w.update_view();
        };
    };
    
    const double * _m;
    const int _m_nrow;
    const int _m_ncol;
    int _row;
    int _col;
    int _nrow;
    int _ncol;
    SlideBoundary _b;
    Iterator _begin;
    Iterator _end;
    
public:
    Anchor anchor;
    
    Window(const double * m, const int nrow, const int ncol):
        _m(m), _m_nrow(nrow), _m_ncol(ncol), 
        _row(0), _col(0), _nrow(nrow), _ncol(ncol),
        _b(*this), _begin(*this), _end(*this), anchor(*this) {
        
        update_view();
    };
    
    Window(const Window& w):
        _m(w._m), _m_nrow(w._m_nrow), _m_ncol(w._m_ncol),
        _row(w._row), _col(w._col), _nrow(w._nrow), _ncol(w._ncol),
        _b(*this), _begin(*this), _end(*this), anchor(*this) {
        
        update_view();
    };
    
    const double& operator[](const int k) const {
        return _m[_b.index(k)];
    };
    
    const Iterator& begin() const {
        return _begin;
    };
    
    const Iterator& end() const {
        return _end;
    };
    
    const int row_from() const {
        return _b.row_from();
    };
    
    const int row_to() const {
        return _b.row_to();
    };
    
    const int col_from() const {
        return _b.col_from();
    };
    
    const int col_to() const {
        return _b.col_to();
    };
    
    const int count() const {
        return (row_to() - row_from() - 1) * (col_to() - col_from() - 1);
    };
    
    const int size() const {
        return _b.size();
    };
    
    const int& matrix_nrow() const {
        return _m_nrow;
    };
    
    const int& matrix_ncol() const {
        return _m_ncol;
    };
    
    const int& row() const {
        return _row;
    };
    
    const int& col() const {
        return _col;
    };
    
    const int& nrow() const {
        return _nrow;
    };
    
    const int& ncol() const {
        return _ncol;
    };
    
    void move(int row, int col) {
        anchor.position(row, col);
    };
    
    void update_view() {
        _b.check_anchor();
        _row = std::max(std::min(anchor.row(), _m_nrow), 0);
        _col = std::max(std::min(anchor.col(), _m_ncol), 0);
        _nrow = std::max(std::min(anchor.nrow() + anchor.row() - _row, _m_nrow - _row), 0);
        _ncol = std::max(std::min(anchor.ncol() + anchor.col() - _col, _m_ncol - _col), 0);
        _end.update_end();
    };
};


// Boundaries classes

class _Boundary {
    
public:
    virtual const int index(const int) const = 0;
    virtual const int row_from() const = 0;
    virtual const int row_to() const = 0;
    virtual const int col_from() const = 0;
    virtual const int col_to() const = 0;
    virtual const int size() const = 0;
    virtual SEXP check_anchor() const = 0;
};

class Soft_Boundary: public _Boundary {
    
private:
    const Window<Soft_Boundary>& _w;
    
public:
    Soft_Boundary(const Window<Soft_Boundary>& w): _w(w) {};
    
    const int index(const int k) const {
        
        return (_w.row() + (k % _w.nrow())) + (_w.col() + (k / _w.nrow())) * _w.matrix_nrow();
    };
    
    const int row_from() const {
        return 1 - _w.anchor.nrow();
    };
    
    const int row_to() const {
        return _w.matrix_nrow();
    };
    
    const int col_from() const {
        return 1 - _w.anchor.ncol();
    };
    
    const int col_to() const {
        return _w.matrix_ncol();
    };
    
    const int size() const {
        return _w.nrow() * _w.ncol();
    };
    
    SEXP check_anchor() const {
        BEGIN_RCPP
        
        END_RCPP
    };
};

class Hard_Boundary: public _Boundary {
    
private:
    const Window<Hard_Boundary>& _w;
    
public:
    Hard_Boundary(const Window<Hard_Boundary>& w): _w(w) {};
    
    const int index(const int k) const {
        return (_w.row() + (k % _w.nrow())) + (_w.col() + (k / _w.nrow())) * _w.matrix_nrow();
    };
    
    const int row_from() const {
        return 0;
    };
    
    const int row_to() const {
        return _w.matrix_nrow() - _w.anchor.nrow() + 1;
    };
    
    const int col_from() const {
        return 0;
    };
    const int col_to() const {
        return _w.matrix_ncol() - _w.anchor.ncol() + 1;
    };
    
    const int size() const {
        return _w.anchor.size();
    };
    
    SEXP check_anchor() const {
        BEGIN_RCPP
        
        if (_w.anchor.row() < 0 || _w.anchor.col() < 0 || 
            _w.anchor.row() + _w.anchor.nrow() > _w.matrix_nrow() ||
            _w.anchor.col() + _w.anchor.ncol() > _w.matrix_ncol()) 
            throw(Rcpp::exception("`Hard_boundary' anchor out of bounds."));

        return R_NilValue;
        
        END_RCPP
    };
};

class Periodic_Boundary: public _Boundary {
    
private:
    const Window<Periodic_Boundary>& _w;
    
public:
    Periodic_Boundary(const Window<Periodic_Boundary>& w): _w(w) {};
    
    const int index(const int k) const {
        return _mod(_w.anchor.row() + k % _w.anchor.nrow(), _w.matrix_nrow()) + 
            _mod(_w.anchor.col() + k / _w.anchor.nrow(), _w.matrix_ncol()) * _w.matrix_nrow();
    };
    
    const int row_from() const {
        return 1 - _w.anchor.nrow();
    };
    
    const int row_to() const {
        return _w.matrix_nrow();
    };
    
    const int col_from() const {
        return 1 - _w.anchor.ncol();
    };
    
    const int col_to() const {
        return _w.matrix_ncol();
    };
    
    const int size() const {
        return _w.anchor.size();
    };

    SEXP check_anchor() const {
        BEGIN_RCPP
        
        END_RCPP
    };
    
private:
    int _mod(const int a, const int b) const {
        return (b + (a % b)) % b;
    };
    
};


// Start of Rcpp functions

// [[Rcpp::export]]
Rcpp::NumericVector win(const Rcpp::NumericMatrix& x, 
                        int i, int j, int i_len, int j_len) {
    
    Window<Hard_Boundary> w(&x[0], x.nrow(), x.ncol());
    
    w.anchor.position(i - 1, j - 1);
    w.anchor.resize(i_len, j_len);
    
    Rcpp::NumericVector result(w.anchor.size());
    
    // for (int k = 0; k < w.size(); ++k)
    //     result[k] = w[k];
    
    int k = 0;
    for (Window<Hard_Boundary>::Iterator it = w.begin(); it != w.end(); ++it)
        result[k++] = *it;
    
    return result;
};

// [[Rcpp::export]]
void slide(const Rcpp::NumericMatrix& x, 
           int i_len, int j_len) {
    
    Window<Periodic_Boundary> w(&x[0], x.nrow(), x.ncol());
    
    w.anchor.resize(i_len, j_len);
    
    Rcpp::NumericVector result(w.anchor.size());
    
    for (int j = w.col_from(); j < w.col_to(); ++j)
        for (int i = w.row_from(); i < w.row_to(); ++i) {
            w.move(i, j);
            
            for (int i = 0; i < result.size(); ++i)
                result[i] = 0;
            
            int k = 0;
            for (Window<Periodic_Boundary>::Iterator it = w.begin(); it != w.end(); ++it)
                 result[k++] = *it;
            
            Rcpp::print(result);
        };
};

// [[Rcpp::export]]
Rcpp::DataFrame costanza(const Rcpp::NumericMatrix& x,
                         const Rcpp::NumericMatrix& y,
                         const Rcpp::NumericVector resolution) {
    BEGIN_RCPP
    
    for (int i = 0; i < resolution.size(); ++i)
        if (resolution[i] < 0) {
            throw(Rcpp::exception("constanza - resolutions must be positive."));
            return R_NilValue;
        };
    
    Window<Hard_Boundary> wx(&x[0], x.nrow(), x.ncol());
    Window<Hard_Boundary> wy(&y[0], y.nrow(), y.ncol());
    
    std::set<double> set_xy(wx.begin(), wx.end());
    set_xy.insert(wy.begin(), wy.end());
    std::vector<double> vset_xy(set_xy.begin(), set_xy.end());
    
    std::map<double, int> count_x;
    std::map<double, int> count_y;
    
    Rcpp::NumericVector result_resolutions;
    Rcpp::NumericVector result_classes;
    Rcpp::NumericVector result_class_acc(vset_xy.size() * resolution.size());
    Rcpp::NumericVector result_global_acc;
                                               
    std::vector<double> valid_cells(vset_xy.size());
    
    for (int l = 0; l < resolution.size(); ++l) {
        
        wx.anchor.position(0, 0);
        wy.anchor.position(0, 0);
        wx.anchor.resize(resolution[l], resolution[l]);
        wy.anchor.resize(resolution[l], resolution[l]);
        
        for (int j = std::max(wx.col_from(), wy.col_from()); j < std::min(wx.col_to(), wy.col_to()); ++j)
            for (int i = std::max(wx.row_from(), wy.row_from()); i < std::min(wx.row_to(), wy.row_to()); ++i) {
                
                wx.move(i, j);
                wy.move(i, j);
                
                for (int k = 0; k < vset_xy.size(); ++k) {
                    count_x[vset_xy[k]] = 0;
                    count_y[vset_xy[k]] = 0;
                };
                
                for (int k = 0; k < wx.size(); ++k) {
                    if (wx[k] == wx[k])
                        ++count_x[wx[k]];
                    if (wy[k] == wy[k])
                        ++count_y[wy[k]];
                };
                
                for (int k = 0; k < vset_xy.size(); ++k) {
                    result_class_acc[k + l * set_xy.size()] += 
                        std::abs(count_x[vset_xy[k]] - count_y[vset_xy[k]]);
                    valid_cells[k] += count_x[vset_xy[k]] + count_y[vset_xy[k]];
                };
            };
        
        double total_error = 0, total_valid_cells = 0;
        for (int k = 0; k < vset_xy.size(); ++k) {
            total_error += result_class_acc[k + l * set_xy.size()];
            total_valid_cells += valid_cells[k];
            result_class_acc[k + l * set_xy.size()] = 
                1.0 - result_class_acc[k + l * set_xy.size()] / valid_cells[k];
        };
        total_error = 1.0 - total_error / total_valid_cells;
        
        for (int i = 0; i < vset_xy.size(); ++i)
            result_global_acc.push_back(total_error);
    };
    
    for (int i = 0; i < resolution.size(); ++i)
        for (int j = 0; j < vset_xy.size(); ++j) {
            result_resolutions.push_back(resolution[i]);
            result_classes.push_back(vset_xy[j]);
        };
    
    Rcpp::DataFrame result = 
        Rcpp::DataFrame::create(Rcpp::Named("resolution") = result_resolutions,
                                Rcpp::Named("classes") = result_classes,
                                Rcpp::Named("global_acc") = result_global_acc, 
                                Rcpp::Named("class_acc") = result_class_acc);
    
    return result;
    
    END_RCPP
};

