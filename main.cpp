#include <iostream>
#include <utility>
#include <vector>
#include <fstream>
#include <cmath>

std::vector<unsigned> split(const std::string &txt)
{
    std::vector<unsigned> numbers;
    size_t pos = txt.find(' ');
    size_t initialPos = 0;

    // Decompose statement
    while( pos != std::string::npos ) {
        numbers.push_back(stoi(txt.substr( initialPos, pos - initialPos ) ));
        initialPos = pos + 1;

        pos = txt.find(' ', initialPos );
    }

    // Add the last one
    numbers.push_back( stoi(txt.substr( initialPos, std::min( pos, txt.size() ) - initialPos + 1 )));

    return numbers;
}
bool hasNumber(const std::vector<unsigned>& array, unsigned value)
{
    for (auto &number : array)
    {
        if (number == value)
        {
            return true;
        }
    }
    return false;
}

long factorial(const int n)
{
    long f = 1;
    for (int i=1; i<=n; ++i)
        f *= i;
    return f;
}

struct Vertex
{
    Vertex(unsigned int index, std::vector<unsigned int> neighbours)
    : index(index),
      neighbours(std::move(neighbours))
    {
    }

    unsigned index;
    std::vector<unsigned> neighbours;

    void invert(unsigned numOfVertex)
    {
        std::vector<unsigned> res;

        for (unsigned i = 1; i <= numOfVertex; i++) //vertex numeration starts from 1!
        {
            if (i != index && !hasNumber(neighbours, i))
            {
                res.push_back(i);
            }
        }
        neighbours = res;
    }

    void eraseEdge(unsigned index)
    {
        for (auto it = neighbours.begin(); it < neighbours.end(); it++)
        {
            if (*it == index)
            {
                neighbours.erase(it);
            }
        }
    }
};

struct CoalitionInfo
{
    std::vector<Vertex> coalition;
    unsigned func;

    bool includesVertex(Vertex search)
    {
        for (auto &vertex : coalition)
        {
            if (vertex.index == search.index)
            {
                return true;
            }
        }

        return false;
    }
};

class Graph
{
public :
    Graph() = default;
    Graph(std::vector<Vertex> vertexes) : m_vertexes(vertexes){}
    Graph(const std::string& filename)
    {
        std::ifstream f(filename);
        unsigned numOfVertex;
        unsigned currentIndex;

        f >> numOfVertex;
        std::string vertexInfo;
        std::getline(f, vertexInfo, '\n');
        for (unsigned i = 0; i < numOfVertex; i++)
        {
            std::getline(f, vertexInfo);
            auto numbers = split(vertexInfo);
            currentIndex = numbers.at(0);
            numbers.erase(numbers.begin());
            m_vertexes.emplace_back(Vertex(currentIndex, numbers));
        }

        f >> delta;
        shepli = std::vector<float>(m_vertexes.size(), 0);
    }

    std::vector<Vertex> m_vertexes;
    std::vector<std::vector<Vertex>> clicks;
    std::vector<CoalitionInfo> coalitions;
    std::vector<float> shepli;
    unsigned delta;

    void findCoalitions(std::vector<Vertex> currentCoalition, unsigned currentIndex)
    {
        if (currentIndex == m_vertexes.size() + 1)
        {
            if (!currentCoalition.empty())
            {
                coalitions.push_back({currentCoalition, 0});
            }
            return;
        }

        findCoalitions(currentCoalition, currentIndex + 1);
        currentCoalition.push_back(getVertexByIndex(m_vertexes,currentIndex));
        findCoalitions(currentCoalition, currentIndex + 1);
    }

    void countCoalitions()
    {
        for (auto &coalition : coalitions)
        {
            auto ws = findClickSize(coalition.coalition);
            unsigned sum = coalition.coalition.size() * delta;
            removeIsolated(coalition.coalition);
            for (unsigned i = 2; i <= ws; i++)
            {
                sum += dfs(i, coalition.coalition) * pow(delta, i);
            }
            coalition.func = sum;
        }
    }
    CoalitionInfo findCoalitionWithoutVertex(std::vector<Vertex> search)
    {
        for (auto &coalition : coalitions)
        {
            if (coalition.coalition.size() != search.size())
                continue;

            bool equal = true;
            for (size_t i = 0; i < search.size(); i++)
            {
                if (coalition.coalition.at(i).index != search.at(i).index)
                {
                    equal = false;
                    break;
                }
            }
            if (equal)
            {
                return coalition;
            }
        }

        return {};
    }
    void countShepli()
    {
        for (unsigned i = 0; i < shepli.size(); i++)
        {
            shepli.at(i) = 0;
            auto currentVertex = getVertexByIndex(m_vertexes, i + 1);
            for (auto &coalition : coalitions)
            {
                if (coalition.includesVertex(currentVertex))
                {
                    auto n = m_vertexes.size();
                    auto m = coalition.coalition.size();
                    float fact = factorial(m - 1) * factorial(n - m) / static_cast<float>(factorial(n));
                    shepli.at(i) += coalition.func * fact;
                    auto newCoalition = coalition.coalition;
                    removeVertex(newCoalition, currentVertex);
                    auto newCoalitionInfo = findCoalitionWithoutVertex(newCoalition);
                    shepli.at(i) -= newCoalitionInfo.func * fact;
                }
            }
        }
    }
    void printShepli()
    {
        for (int i = 0; i < shepli.size(); i++)
        {
            std::cout << "Shepli for " << i + 1 << " = " << shepli.at(i) << '\n';
        }
    }

    void removeVertex(std::vector<Vertex> &vertexes, Vertex vertex)
    {
        for (auto it = vertexes.begin(); it < vertexes.end(); it++)
        {
            if ((*it).index == vertex.index)
            {
                vertexes.erase(it);
            }
            else
            {
                (*it).eraseEdge(vertex.index);
            }
        }
    }

    Vertex getVertexByIndex(std::vector<Vertex> vertexes, unsigned index)
    {
        for (auto &vertex : vertexes)
        {
            if (vertex.index == index)
            {
                return vertex;
            }
        }

        return Vertex(0, {}); //in case of not found
    }

    void print()
    {
        for (auto &vertex : m_vertexes)
        {
            std::cout << "Vertex " << vertex.index;
            if (!vertex.neighbours.empty())
            {
                std::cout << " Neighbours ";
                for (auto &neighbour : vertex.neighbours)
                {
                    std::cout << neighbour << ' ';
                }
            }
            std::cout << '\n';
        }
    }

    Graph invert()
    {
        Graph res;
        res.m_vertexes = m_vertexes;
        auto numOfVertex = m_vertexes.size();

        for (auto &vertex : res.m_vertexes)
        {
            vertex.invert(numOfVertex);
        }

        return res;
    }

    unsigned findClickSize(std::vector<Vertex> vertexes)
    {
        std::vector<Vertex> candidates = vertexes;
        std::vector<Vertex> notCandidates;
        std::vector<Vertex> click;

        clicks.clear();
        extend(candidates, notCandidates, click);

        auto maxClickSize = clicks.empty() ? 0 : clicks.at(0).size();

        for (auto &click : clicks)
        {
            if (click.size() > maxClickSize)
            {
                maxClickSize = click.size();
            }
        }

        return maxClickSize;
    }

    void removeIsolated(std::vector<Vertex> &vertexes)
    {
        auto it = vertexes.begin();

        while (it != vertexes.end())
        {
            if ((*it).neighbours.empty())
            {
                it = vertexes.erase(it);
            }
            else
            {
                ++it;
            }
        }
    }

    unsigned dfs(unsigned k, std::vector<Vertex> startVertex)
    {
        unsigned v = 0;
        auto copy1 = startVertex;
        for (auto &vertex : startVertex)
        {
            step(k, 1, {vertex}, v, copy1);
            removeVertex(copy1, vertex);
        }

        return v;
    }

private:
    void extend(std::vector<Vertex> candidates, std::vector<Vertex> notCandidates, std::vector<Vertex> click)
    {
        for (auto candidateIt = candidates.begin(); candidateIt < candidates.end(); candidateIt++)
        {
            auto currentVertex = *candidateIt;
            click.push_back(currentVertex);
            candidates.erase(candidateIt);

            std::vector<Vertex> newCandidates, newNotCandidates;

            for (auto &vertex : candidates)
            {
                if (hasNumber(vertex.neighbours, currentVertex.index))
                {
                    newCandidates.push_back(vertex);
                }
            }

            for (auto &vertex : notCandidates)
            {
                if (hasNumber(vertex.neighbours, currentVertex.index))
                {
                    newNotCandidates.push_back(vertex);
                }
            }

            if (newCandidates.empty() && newNotCandidates.empty())
            {
                clicks.push_back(click);
            }
            else
            {
                extend(newCandidates, newNotCandidates, click);
            }

            click.erase(click.end() - 1);
            notCandidates.push_back(currentVertex);
        }
    }
    void step(unsigned k, unsigned currentStep, std::vector<Vertex> path, unsigned &v, std::vector<Vertex> copy2)
    {
        if (k == currentStep)
        {
            auto lastVertex = path.back();

            for (auto &step : path)
            {
                if (!hasNumber(lastVertex.neighbours, step.index) && lastVertex.index != step.index)
                    return;
            }

            v += k;
//            for (auto &vertex : path)
//            {
//                std::cout << vertex.index << ' ';
//            }
//            std::cout << '\n';
            return;
        }

        else
        {
            for (auto &vertexIndex : path.back().neighbours)
            {
                bool visitedVertex = false;
                for (auto &oldStep : path)
                {
                    if (oldStep.index == vertexIndex)
                    {
                        visitedVertex = true;
                    }
                }
                if (visitedVertex)
                {
                    continue;
                }

                auto newPath = path;
                auto vertex = getVertexByIndex(copy2,vertexIndex);
                newPath.push_back(vertex);
                step(k, currentStep + 1, newPath, v, copy2);
                removeVertex(copy2, vertex);
            }
        }
    }
};


int main() {
    Graph g("f.txt");
    g.findCoalitions({}, 1);
    g.countCoalitions();
    g.countShepli();
    g.printShepli();
}
