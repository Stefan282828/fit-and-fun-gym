import React, { useState, useEffect } from 'react';
import { User, PageResponse } from '../../types';
import { userApi } from '../../services/api';
import { Plus, Edit, Trash2, UserPlus, Check } from 'lucide-react';

interface UserFormProps {
  user: User | null;
  onClose: () => void;
  onSuccess: () => void;
}

// Dummy UserForm komponenta, zameni sa pravom implementacijom
const UserForm: React.FC<UserFormProps> = ({ user, onClose, onSuccess }) => {
  return (
    <div className="modal">
      <div className="modal-content">
        <h2>{user ? 'Edit User' : 'Add User'}</h2>
        {/* Forma ovde */}
        <button onClick={() => { onSuccess(); }}>Save</button>
        <button onClick={onClose}>Cancel</button>
      </div>
    </div>
  );
};

interface AssignTrainingPlanModalProps {
  userId: number;
  onClose: () => void;
  onSuccess: () => void;
}

// Dummy AssignTrainingPlanModal, zameni sa pravom implementacijom
const AssignTrainingPlanModal: React.FC<AssignTrainingPlanModalProps> = ({ userId, onClose, onSuccess }) => {
  return (
    <div className="modal">
      <div className="modal-content">
        <h2>Assign Training Plan to User ID: {userId}</h2>
        {/* Forma za dodeljivanje trening plana */}
        <button onClick={() => { onSuccess(); }}>Assign</button>
        <button onClick={onClose}>Cancel</button>
      </div>
    </div>
  );
};

const UserList: React.FC = () => {
  const [users, setUsers] = useState<PageResponse<User>>({
    content: [],
    totalElements: 0,
    totalPages: 0,
    size: 10,
    number: 0,
  });
  const [loading, setLoading] = useState(true);
  const [showForm, setShowForm] = useState(false);
  const [editingUser, setEditingUser] = useState<User | null>(null);
  const [showAssignModal, setShowAssignModal] = useState(false);
  const [selectedUserId, setSelectedUserId] = useState<number | null>(null);
  const [currentPage, setCurrentPage] = useState(0);

const fetchUsers = async (page = currentPage) => {
  setLoading(true);
  try {
    const response = await userApi.getAll(page, 10);
    console.log('API response:', response.data);
    setUsers(response.data);
    setCurrentPage(page);
  } catch (error) {
    console.error('Error fetching users:', error);
  } finally {
    setLoading(false);
  }
};


  useEffect(() => {
    fetchUsers(currentPage);
  }, [currentPage]);

  const handleDelete = async (id: number) => {
    if (window.confirm('Are you sure you want to delete this user?')) {
      try {
        await userApi.delete(id);
        fetchUsers(currentPage);
      } catch (error) {
        console.error('Error deleting user:', error);
      }
    }
  };

  const handleEdit = (user: User) => {
    setEditingUser(user);
    setShowForm(true);
  };

  const handleAssignTrainingPlan = (userId: number) => {
    setSelectedUserId(userId);
    setShowAssignModal(true);
  };

  const handleFinishTrainingPlan = async (userId: number) => {
    if (window.confirm('Are you sure you want to finish this user\'s training plan?')) {
      try {
        await userApi.finishTrainingPlan(userId);
        alert('Training plan finished successfully!');
        fetchUsers(currentPage);
      } catch (error) {
        console.error('Error finishing training plan:', error);
      }
    }
  };

  const getRoleBadgeColor = (role: string) => {
    switch (role) {
      case 'ADMIN': return 'bg-red-100 text-red-800';
      case 'COACH': return 'bg-blue-100 text-blue-800';
      case 'USER': return 'bg-green-100 text-green-800';
      default: return 'bg-gray-100 text-gray-800';
    }
  };

  if (loading) {
    return (
      <div className="flex justify-center items-center h-64">
        <div className="animate-spin rounded-full h-12 w-12 border-b-2 border-primary-600"></div>
      </div>
    );
  }

  return (
    <div className="max-w-7xl mx-auto px-4 sm:px-6 lg:px-8 py-8 space-y-8">
      <div className="flex justify-between items-center">
        <div>
          <h1 className="text-3xl font-bold text-gray-900">Users Management</h1>
          <p className="text-gray-600 mt-1">Manage gym members, coaches, and administrators</p>
        </div>
        <button onClick={() => setShowForm(true)} className="btn-primary flex items-center space-x-2">
          <Plus className="h-4 w-4" />
          <span>Add User</span>
        </button>
      </div>

      <div className="card">
        <div className="overflow-hidden">
          <table className="min-w-full divide-y divide-gray-200">
            <thead className="bg-gray-50">
              <tr>
                <th className="px-6 py-4 text-left text-xs font-semibold text-gray-600 uppercase tracking-wider">
                  User
                </th>
                <th className="px-6 py-4 text-left text-xs font-semibold text-gray-600 uppercase tracking-wider">
                  Email
                </th>
                <th className="px-6 py-4 text-left text-xs font-semibold text-gray-600 uppercase tracking-wider">
                  Role
                </th>
                <th className="px-6 py-4 text-left text-xs font-semibold text-gray-600 uppercase tracking-wider">
                  Date of Birth
                </th>
                <th className="px-6 py-4 text-right text-xs font-semibold text-gray-600 uppercase tracking-wider">
                  Actions
                </th>
              </tr>
            </thead>
            <tbody className="bg-white divide-y divide-gray-200">
              {users.content && users.content.length > 0 ? (
                users.content.map((user) => (
                  <tr key={user.id} className="hover:bg-gray-50 transition-colors">
                    <td className="px-6 py-4 whitespace-nowrap">
                      <div>
                        <div className="font-semibold text-gray-900">
                          {user.name} {user.lastName}
                        </div>
                        <div className="text-sm text-gray-500">@{user.username}</div>
                      </div>
                    </td>
                    <td className="px-6 py-4 whitespace-nowrap text-gray-900">
                      {user.email}
                    </td>
                    <td className="px-6 py-4 whitespace-nowrap">
                      <span className={`inline-flex px-3 py-1 text-xs font-semibold rounded-full ${getRoleBadgeColor(user.role)}`}>
                        {user.role}
                      </span>
                    </td>
                    <td className="px-6 py-4 whitespace-nowrap text-gray-900">
                      {new Date(user.dateOfBirth).toLocaleDateString()}
                    </td>
                    <td className="px-6 py-4 whitespace-nowrap text-right space-x-3">
                      <button
                        onClick={() => handleEdit(user)}
                        className="text-primary-600 hover:text-primary-700 transition-colors"
                      >
                        <Edit className="h-4 w-4" />
                      </button>
                      <button
                        onClick={() => handleAssignTrainingPlan(user.id)}
                        className="text-green-600 hover:text-green-700 transition-colors"
                      >
                        <UserPlus className="h-4 w-4" />
                      </button>
                      <button
                        onClick={() => handleFinishTrainingPlan(user.id)}
                        className="text-yellow-600 hover:text-yellow-700 transition-colors"
                      >
                        <Check className="h-4 w-4" />
                      </button>
                      <button
                        onClick={() => handleDelete(user.id)}
                        className="text-red-600 hover:text-red-700 transition-colors"
                      >
                        <Trash2 className="h-4 w-4" />
                      </button>
                    </td>
                  </tr>
                ))
              ) : (
                <tr>
                  <td colSpan={5} className="text-center py-4 text-gray-500">
                    No users found.
                  </td>
                </tr>
              )}
            </tbody>
          </table>
        </div>

        {/* Pagination */}
        <div className="flex items-center justify-between px-6 py-4 bg-gray-50 border-t border-gray-200">
          <div className="text-sm text-gray-600">
            Showing {users.number * users.size + 1} to {Math.min((users.number + 1) * users.size, users.totalElements)} of {users.totalElements} results
          </div>
          <div className="flex space-x-2">
            <button
              onClick={() => fetchUsers(Math.max(0, currentPage - 1))}
              disabled={currentPage === 0}
              className="btn-secondary disabled:opacity-50 disabled:cursor-not-allowed"
            >
              Previous
            </button>
            <button
              onClick={() => fetchUsers(Math.min(users.totalPages - 1, currentPage + 1))}
              disabled={currentPage >= users.totalPages - 1}
              className="btn-secondary disabled:opacity-50 disabled:cursor-not-allowed"
            >
              Next
            </button>
          </div>
        </div>
      </div>

      {showForm && (
        <UserForm
          user={editingUser}
          onClose={() => {
            setShowForm(false);
            setEditingUser(null);
          }}
          onSuccess={() => {
            fetchUsers(currentPage);
            setShowForm(false);
            setEditingUser(null);
          }}
        />
      )}

      {showAssignModal && selectedUserId !== null && (
        <AssignTrainingPlanModal
          userId={selectedUserId}
          onClose={() => {
            setShowAssignModal(false);
            setSelectedUserId(null);
          }}
          onSuccess={() => {
            fetchUsers(currentPage);
            setShowAssignModal(false);
            setSelectedUserId(null);
          }}
        />
      )}
    </div>
  );
};

export default UserList;
