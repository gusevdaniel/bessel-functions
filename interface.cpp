#include <iostream>

#include <cstdio>  
#include <cstdlib>
#include <cstring>
#include <sstream>
#include <fstream>

using namespace std;

string GetString(string txt) {
    ifstream F(txt);
    string line;
    if (!F.is_open()) {
        cout << "Error: File can not be open\n";
    }
    else {
            getline(F, line);
    }
    F.close();
    return line;
}

void StringToFile(string buff) {
    ofstream F("massive.txt");
    F << buff;
}

void Notification(){
    cout << "Calculation is completed" << endl;
    cout << "Answer in massive.txt" << endl;
}

void help()
{
	cout << "Format: function (-b, -n, -h, -hh) v x or v_min v_max x_min x_max step (0.1)" << endl;
}

int main(int argc, char* argv[]) 
{
    string temp = "result.txt";

    if (argc == 4) {
		stringstream stream;
		stream << "\"bessel-functions.exe\"" << " " << argv[1] << " " << argv[2] << " " << argv[3];
		system(stream.str().c_str());

        StringToFile(GetString(temp) + "\n");
        remove(temp.c_str());
        Notification();
    }
    else if (argc == 7) {
        string buff = "";
        int v_min = atoi(argv[2]);
        int v_max = atoi(argv[3]);
        double step = atof(argv[6]);

        if (step == 0) {
            cout << "Error: step = 0" << endl;
            exit(0);
        }

        for (v_min; v_min <= v_max; v_min++) {
            double x_min = atoi(argv[4]);
            double x_max = atoi(argv[5]);

            for (x_min; x_min <= x_max; x_min=x_min+step) {
                stringstream stream;
                stream << "\"bessel-functions.exe\"" << " " << argv[1] << " " << v_min << " " << x_min;
                system(stream.str().c_str());
                buff = buff + GetString(temp) + "\n";
            }

            if (v_min != v_max) {
                buff = buff + "\n";
            }
        }

        int p=1;
        while (p >= 0)
        {
            p = buff.find("(");
            if (p > 0) { buff.erase(p, 1);}

            p = buff.find(")");
            if (p > 0) { buff.erase(p, 1); }
        }

        StringToFile(buff);
        remove(temp.c_str());
        Notification();
    }
    else {
        help();
    }
}