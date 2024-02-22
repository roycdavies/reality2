# ------------------------------------------------------------------------------------------------------------------------------------------------------
# A transition for the FSM / Automation
# ------------------------------------------------------------------------------------------------------------------------------------------------------
class Transition:
	var _from: String
	var _event: String
	var _to: String
	var _actions = []
	
	func _init(from, event, to, actions = []):
		_from = from
		_event = event
		_to = to
		_actions = actions
		
	func from(): return _from
	func event(): return _event
	func to(): return _to
	func actions(): return _actions
# ------------------------------------------------------------------------------------------------------------------------------------------------------



# ------------------------------------------------------------------------------------------------------------------------------------------------------
# A Finite State Machine or Automation
# ------------------------------------------------------------------------------------------------------------------------------------------------------
class FSM:
	var _current_state: String
	var _transitions = []
	var _event_queue = []
	var _timed_events = []
	
	# --------------------------------------------------------------------------------------------------------------------------------------------------
	# Constructor
	# --------------------------------------------------------------------------------------------------------------------------------------------------
	func _init():
		_current_state = "start"
		queue_event("init", {})
	# --------------------------------------------------------------------------------------------------------------------------------------------------
	
	
	# --------------------------------------------------------------------------------------------------------------------------------------------------
	# Public functions
	# --------------------------------------------------------------------------------------------------------------------------------------------------
	func add_transition(from, event, to, actions):
		var transition = Transition.new(from, event, to, actions)

	func queue_event(event: String, parameters = {}, delay = 0):
		if (delay == 0):
			_event_queue.push_back({"event": event, "parameters": parameters})
		else:
			_timed_events.push_back({"event": event, "parameters": parameters, "time": Time.get_ticks_msec() + delay * 1000})
	
	# Should be called every frame (eg in the _process function of a node)
	func step():
		# Check the timed events and queue any that have expired
		for i in range(_timed_events.size() - 1, -1, -1):
			if (_timed_events[i].time <= Time.get_ticks_msec()):
				queue_event(_timed_events[i].event, _timed_events[i].parameters)
				_timed_events.remove(i)
		
		# Check the events and perform transitions
		if _event_queue.size() > 0:
			var event_and_parameters = _event_queue.pop_front()
			_check_transitions(event_and_parameters)
	# --------------------------------------------------------------------------------------------------------------------------------------------------
	
	
	# --------------------------------------------------------------------------------------------------------------------------------------------------
	# Private functions
	# --------------------------------------------------------------------------------------------------------------------------------------------------
	func _check_transitions(event_and_parameters):
		for transition in _transitions:
			if (_current_state == transition.from() || transition.from() == "*"):
				if (event_and_parameters.event == transition.event()):
					if(transition.to() != "*"):
						_current_state = transition.to()
					var previous_result = event_and_parameters.parameters
					for action in transition.actions():
						previous_result = action.call(previous_result)
					break
	# --------------------------------------------------------------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------------------------------------------------------------------
