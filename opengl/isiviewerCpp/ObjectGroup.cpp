#include "ObjectGroup.h"
#include <functional>
#include <iostream>

ObjectGroup::ObjectGroup()
{
}

ObjectGroup::ObjectGroup(const ObjectGroup& other)
	: Object(other)
	, _parts(other._parts.size())
{
	std::transform( other._parts.begin(), other._parts.end(), _parts.begin(), std::mem_fun(&Object::clone));
}

ObjectGroup::~ObjectGroup()
{
	for ( std::vector<Object*>::iterator it = _parts.begin(); it != _parts.end(); ++it)
	{
		delete (*it);
	}
}

void ObjectGroup::dump(std::ostream& output) const
{
	output << "=========== begin object group ================" << std::endl;

	for ( std::vector<Object*>::const_iterator it = _parts.begin(); it != _parts.end(); ++it)
	{
		(*it)->dump(output);
	}
	
	output << "=========== end object group ================" << std::endl;
}

ObjectGroup* ObjectGroup::clone() const
{
	return new ObjectGroup(*this);
}

void ObjectGroup::add(Object* part)
{
	_parts.push_back(part);
}

void ObjectGroup::drawShape(bool smooth) const
{
	for ( std::vector<Object*>::const_iterator it = _parts.begin(); it != _parts.end(); ++it)
	{
		(*it)->draw(smooth);
	}
}
