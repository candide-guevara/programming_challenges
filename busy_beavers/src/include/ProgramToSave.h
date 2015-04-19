/*
 * Created on: Nov 25, 2013
 *      Author: eguevara
 */

#ifndef PROGRAMTOSAVE_H_
#define PROGRAMTOSAVE_H_

#include <vector>
#include <ProgramState.h>
#include <ExeStats.h>
class ProgramMap;

/**
 * Container for the program states that should be saved for later.
 * Implemented as a circular list to save the overhead of cleaning it
 */
class ProgramToSave {
  public:
    ProgramToSave();
    ProgramToSave(const BigInteger, const BigInteger);

    void maybeSaveForLater(const ProgramState& iState, const ExeStats& iStats);
    void absolutelySaveForLater(const ProgramState& iState, const ExeStats& iStats);
    void saveIntoMap(ProgramMap& iMap, const ExeStats& iFinalStats);

    void setSaveUpto(const BigInteger isaveUpto);
    void setSaveDropRatio(const BigInteger isaveDropRatio);

    void clear();
    BigInteger size() const;

    typedef std::pair<ProgramState, ExeStats> InnerItem;
    class iterator {
      public:
        iterator(BigInteger iIdx, const ProgramToSave* iData) : idx(iIdx), data(iData) {}
        iterator& operator -- ();
        iterator& operator ++ ();
        bool operator == (const iterator& iOther);
        bool operator != (const iterator& iOther);
        const InnerItem& operator *() const;

      private:
        BigInteger idx;
        const ProgramToSave *data;
    };

    friend class iterator;
    iterator begin() const;
    iterator end() const;

    friend std::ostream& operator <<(std::ostream& oOs, const ProgramToSave& iItem);

  private:

    std::vector<InnerItem> toSave;
    BigInteger saveUpTo, saveDropRatio;
    BigInteger saveIfNull, head, tail;
};

#endif /* PROGRAMTOSAVE_H_ */
