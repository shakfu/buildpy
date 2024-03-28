#include <iostream>


class Shape {
public:
    virtual void draw() = 0; // pure virtual function

    virtual void draw2()
    {
        std::cout << "Drawing a shape" << std::endl;        
    }
};

class Circle : public Shape {
public:
    void draw() {
        std::cout << "Drawing a circle" << std::endl;
    }
};


int main()
{
    // Shape* s = new Circle;
    // s->draw(); // will print "Drawing a circle"
    // s->draw2(); // will print "Drawing a shape"

    Circle().draw();


    return 0;
}
