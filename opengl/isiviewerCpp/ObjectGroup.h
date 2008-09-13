#ifndef OBJECTGROUP_H_
#define OBJECTGROUP_H_

#include "Object.h"

class ObjectGroup : public Object
{
	std::vector<Object*> _parts;
	
public:
	ObjectGroup();
	ObjectGroup(const ObjectGroup& other);
	virtual ~ObjectGroup();
	
public:
	virtual void dump(std::ostream& output) const;
	
	virtual ObjectGroup* clone() const;
	
	// this keeps the part 
	void add(Object* part);
	
protected:
	virtual void drawShape(bool smooth) const;
};

#endif /*OBJECTGROUP_H_*/
